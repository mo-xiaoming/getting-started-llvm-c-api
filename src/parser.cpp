#include "parser.hpp"

#include <llvm/ADT/STLExtras.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/ValueSymbolTable.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/TargetSelect.h>

#include <cassert>
#include <charconv>
#include <map>

namespace codegen {
struct scope_t {
  std::size_t m_parent = std::numeric_limits<std::size_t>::max();
  std::string_view name;
  std::map<std::string_view, llvm::ConstantInt*> m_consts;
  std::map<std::string_view, llvm::AllocaInst*> m_vars;
  std::map<std::string_view, llvm::Function*> m_funcs;

  [[maybe_unused]] friend std::ostream& operator<<(std::ostream& os, scope_t const& c) {
    os << "const table\n";
    for (auto const& p : c.m_consts) {
      os << "  " << p.first << ':' << llvm::dyn_cast<llvm::ConstantInt>(p.second)->getSExtValue() << '\n';
    }
    os << "var table\n";
    for (auto const& p : c.m_vars) {
      os << "  " << p.first << ':' << llvm::dyn_cast<llvm::ConstantInt>(p.second)->getSExtValue() << '\n';
    }
    os << "func table\n";
    for (auto const& p : c.m_funcs) {
      os << "  " << p.first << ':' << static_cast<void const*>(p.second) << '\n';
    }
    return os;
  }
};

namespace {
[[nodiscard]] bool has_parent(scope_t const& scope) {
  return scope.m_parent != std::numeric_limits<decltype(scope.m_parent)>::max();
}
} // namespace

struct codegen_t {
  codegen_t() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();

    auto* out_fn = create_std_out();
    add_fn_to_scope("out", out_fn);
  }

  void compile_env(parser::environment_t const& env) {
    for (auto const& c : env.consts) {
      add_const_to_scope(parser::sv(c.m_ident), llvm::ConstantInt::get(m_int_type, parser::sv(c.m_num), 10));
    }
    for (auto const& v : env.vars) {
      add_var_to_scope(parser::sv(v.m_ident), m_builder->CreateAlloca(m_int_type, nullptr, parser::sv(v.m_ident)));
    }
    for (auto const& p : env.procedures) {
      auto* proto = llvm::FunctionType::get(llvm::Type::getVoidTy(*m_context), /*isVarArg*/ false);

      auto* fn = llvm::Function::Create(proto, llvm::Function::ExternalLinkage, parser::sv(p.m_ident), m_module.get());
      auto* bb_prev = m_builder->GetInsertBlock();
      auto* bb = llvm::BasicBlock::Create(*m_context, "entry", fn);
      if (bb == nullptr) {
        fn->eraseFromParent();
        assert(false); // NOLINT
      }
      m_builder->SetInsertPoint(bb);

      m_scopes.emplace_back();
      m_scopes.back().m_parent = m_cur_scope;
      m_cur_scope = m_scopes.size() - 1;
      m_scopes[m_cur_scope].name = parser::sv(p.m_ident);
      compile_env(*p.m_env);
      m_cur_scope = m_scopes[m_cur_scope].m_parent;

      m_builder->CreateRetVoid();

      auto& os = llvm::errs();
      if (llvm::verifyFunction(*fn, &os)) {
        fn->eraseFromParent();
        assert(false); // NOLINT
      }

      m_builder->SetInsertPoint(bb_prev);

      add_fn_to_scope(parser::sv(p.m_ident), fn);
    }
    for (auto const& s : env.statements) {
      s->codegen(*this);
    }
  }

  llvm::Function* create_std_out() {
    auto* out =
        dyn_cast<llvm::Function>(m_module->getOrInsertFunction("out", m_builder->getVoidTy(), m_int_type).getCallee());

    auto* bb = llvm::BasicBlock::Create(*m_context, "entry", out);
    m_builder->SetInsertPoint(bb);

    auto printf_fn = m_module->getOrInsertFunction(
        "printf", llvm::FunctionType::get(m_builder->getInt32Ty(), llvm::PointerType::get(m_builder->getInt8Ty(), 0),
                                          /*isVarArg*/ true));
    auto* val = &*out->arg_begin();
    auto* fmt = m_builder->CreateGlobalStringPtr("%ld\n", "printffmt");
    m_builder->CreateCall(printf_fn, {fmt, val});
    m_builder->CreateRetVoid();

    if (llvm::verifyFunction(*out, &llvm::errs())) {
      return nullptr;
    }
    return out;
  }

  std::unique_ptr<llvm::LLVMContext> m_context{std::make_unique<llvm::LLVMContext>()};
  std::unique_ptr<llvm::Module> m_module{std::make_unique<llvm::Module>("jit-main-module", *m_context)};
  std::unique_ptr<llvm::IRBuilder<>> m_builder{std::make_unique<llvm::IRBuilder<>>(*m_context)};

  llvm::IntegerType* m_int_type = m_builder->getInt64Ty();

  void add_fn_to_scope(std::string_view name, llvm::Function* fn) {
    assert(fn != nullptr); // NOLINT
    m_scopes[m_cur_scope].m_funcs[name] = fn;
  }
  void add_const_to_scope(std::string_view name, llvm::ConstantInt* value) {
    assert(value != nullptr); // NOLINT
    m_scopes[m_cur_scope].m_consts[name] = value;
  }
  void add_var_to_scope(std::string_view name, llvm::AllocaInst* value) {
    assert(value != nullptr); // NOLINT
    m_scopes[m_cur_scope].m_vars[name] = value;
  }

  template <typename R, typename C> R* find_name(C const& c, std::string_view name) {
    if (auto const it = std::find_if(c.cbegin(), c.cend(), [name](auto const& p) { return p.first == name; });
        it != c.cend()) {
      return it->second;
    }
    return nullptr;
  };

  llvm::ConstantInt* find_const(std::string_view name) {
    auto const* cur_scope = &m_scopes[m_cur_scope];
    while (true) {
      if (auto* v = find_name<llvm::ConstantInt>(cur_scope->m_consts, name); v != nullptr) {
        return v;
      }
      if (!has_parent(*cur_scope)) {
        break;
      }
      cur_scope = &m_scopes[cur_scope->m_parent];
    };
    return nullptr;
  }

  llvm::AllocaInst* find_var(std::string_view name) {
    auto const* cur_scope = &m_scopes[m_cur_scope];
    while (true) {
      if (auto* v = find_name<llvm::AllocaInst>(cur_scope->m_vars, name); v != nullptr) {
        return v;
      }
      if (!has_parent(*cur_scope)) {
        break;
      }
      cur_scope = &m_scopes[cur_scope->m_parent];
    };
    return nullptr;
  }

  llvm::Function* find_function(std::string_view name) {
    auto const* cur_scope = &m_scopes[m_cur_scope];
    while (true) {
      // current procedure
      // don't support recursion
      if (cur_scope->m_parent != std::numeric_limits<std::size_t>::max() && cur_scope->name == name) {
        return nullptr;
      }
      if (auto* v = find_name<llvm::Function>(cur_scope->m_funcs, name); v != nullptr) {
        return v;
      }
      if (!has_parent(*cur_scope)) {
        break;
      }
      cur_scope = &m_scopes[cur_scope->m_parent];
    };
    return nullptr;
  }

  std::vector<scope_t> m_scopes{1};
  std::size_t m_cur_scope{0};
};
} // namespace codegen
namespace parser {
std::ostream& operator<<(std::ostream& os, parse_error_t const& pe) {
  return std::visit(
      [&os]<typename T>(T const& e) -> std::ostream& {
        if constexpr (std::is_same_v<parse_error_ok_t, T>) {
          return os << "parse_error_ok_t";
        } else if constexpr (std::is_same_v<parse_error_empty_file_t, T>) {
          return os << "parse_error_empty_file_t";
        } else if constexpr (std::is_same_v<parse_error_early_eof_t, T>) {
          return os << "parse_error_early_eof_t: " << e;
        } else if constexpr (std::is_same_v<parse_error_unexpected_t, T>) {
          return os << "parse_error_unexpected_t: " << e;
        } else if constexpr (std::is_same_v<parse_error_name_redefined_t, T>) {
          return os << "parse_error_name_redefined_t: " << e;
        } else if constexpr (std::is_same_v<parse_error_name_undefined_t, T>) {
          return os << "parse_error_name_undefined_t: " << e;
        } else {
          __builtin_unreachable();
        }
      },
      pe);
}

void ast_t::codegen() const {
  codegen::codegen_t cg;
  auto* main_fn =
      llvm::dyn_cast<llvm::Function>(cg.m_module->getOrInsertFunction("main", cg.m_builder->getVoidTy()).getCallee());
  auto* bb = llvm::BasicBlock::Create(*cg.m_context, "entry", main_fn);
  cg.m_builder->SetInsertPoint(bb);
  cg.compile_env(m_top_env);
  cg.m_builder->CreateRetVoid();
  assert(!llvm::verifyFunction(*main_fn, &llvm::errs()));

  cg.m_module->print(llvm::errs(), nullptr);

  std::unique_ptr<llvm::ExecutionEngine> ee{llvm::EngineBuilder(std::move(cg.m_module)).create()};
  [[maybe_unused]] auto ret = ee->runFunction(ee->FindFunctionNamed("main"), {});
}

llvm::Value* expression_binary_op_t::codegen(codegen::codegen_t& cg) const {
  auto* lhs = m_lhs->codegen(cg);
  auto* rhs = m_rhs->codegen(cg);

  using enum lexer::symbol_t;
  switch (m_op.symbol) {
  case plus:
    return cg.m_builder->CreateAdd(lhs, rhs, "addtmp");
  case minus:
    return cg.m_builder->CreateSub(lhs, rhs, "subtmp");
  case times:
    return cg.m_builder->CreateMul(lhs, rhs, "multmp");
  case divide:
    return cg.m_builder->CreateSDiv(lhs, rhs, "divtmp");
  default:
    __builtin_unreachable();
  }
}

llvm::Value* number_t::codegen(codegen::codegen_t& cg) const {
  return llvm::ConstantInt::get(cg.m_int_type, sv(m_value), 10);
}

llvm::Value* ident_t::codegen(codegen::codegen_t& cg) const {
  if (auto* value = cg.find_const(sv(m_name)); value != nullptr) {
    return value;
  }
  if (auto* value = cg.find_var(sv(m_name)); value != nullptr) {
    return cg.m_builder->CreateLoad(cg.m_int_type, value, sv(m_name));
  }
  return nullptr;
}

void call_t::codegen(codegen::codegen_t& cg) const {
  auto* func = cg.find_function(parser::sv(m_ident));
  cg.m_builder->CreateCall(func);
}

void in_t::codegen(codegen::codegen_t& /*cg*/) const {}

void out_t::codegen(codegen::codegen_t& cg) const {
  auto* func = cg.find_function("out");

  cg.m_builder->CreateCall(func, {m_expression->codegen(cg)});
}

void becomes_t::codegen(codegen::codegen_t& cg) const {
  if (auto* value = cg.find_var(sv(m_name)); value != nullptr) {
    cg.m_builder->CreateStore(m_expression->codegen(cg), value);
  }
}

llvm::Value* odd_condition_t::codegen(codegen::codegen_t& cg) const {
  return cg.m_builder->CreateICmpNE(m_expression->codegen(cg), cg.m_builder->getInt64(0), "oddtmp");
}

llvm::Value* cmp_condition_t::codegen(codegen::codegen_t& cg) const {
  auto* lhs = m_lhs->codegen(cg);
  auto* rhs = m_rhs->codegen(cg);

  using enum lexer::symbol_t;
  switch (m_op.symbol) {
  case greater:
    return cg.m_builder->CreateICmpSGT(lhs, rhs, "gttmp");
  case greater_equal:
    return cg.m_builder->CreateICmpSGE(lhs, rhs, "getmp");
  case less:
    return cg.m_builder->CreateICmpSLT(lhs, rhs, "lttmp");
  case less_equal:
    return cg.m_builder->CreateICmpSGE(lhs, rhs, "letmp");
  case equal:
    return cg.m_builder->CreateICmpEQ(lhs, rhs, "eqtmp");
  case not_equal:
    return cg.m_builder->CreateICmpNE(lhs, rhs, "netmp");
  default:
    __builtin_unreachable();
  }
}

void if_then_t::codegen(codegen::codegen_t& cg) const {
  auto* cond = m_condition->codegen(cg);
  auto* fn = cg.m_builder->GetInsertBlock()->getParent();

  auto* bb_then = llvm::BasicBlock::Create(*cg.m_context, "ifthen", fn);
  auto* bb_phi = llvm::BasicBlock::Create(*cg.m_context, "ifphi");
  cg.m_builder->CreateCondBr(cond, bb_then, bb_phi);

  cg.m_builder->SetInsertPoint(bb_then);
  m_statement->codegen(cg);
  cg.m_builder->CreateBr(bb_phi);

  fn->getBasicBlockList().push_back(bb_phi);
  cg.m_builder->SetInsertPoint(bb_phi);
}

void while_do_t::codegen(codegen::codegen_t& cg) const {
  auto* bb_cond = llvm::BasicBlock::Create(*cg.m_context, "whilecond");
  cg.m_builder->CreateBr(bb_cond);

  auto* fn = cg.m_builder->GetInsertBlock()->getParent();
  fn->getBasicBlockList().push_back(bb_cond);
  cg.m_builder->SetInsertPoint(bb_cond);

  auto* cond = m_condition->codegen(cg);

  auto* bb_body = llvm::BasicBlock::Create(*cg.m_context, "whilebody", fn);
  auto* bb_phi = llvm::BasicBlock::Create(*cg.m_context, "whilephi", fn);
  cg.m_builder->CreateCondBr(cond, bb_body, bb_phi);

  cg.m_builder->SetInsertPoint(bb_body);
  m_statement->codegen(cg);

  cg.m_builder->CreateBr(bb_cond);
  fn->getBasicBlockList().push_back(bb_phi);
  cg.m_builder->SetInsertPoint(bb_phi);
}

void begin_end_t::codegen(codegen::codegen_t& cg) const {
  for (auto const& s : m_statements) {
    s->codegen(cg);
  }
}

template <std::integral NumberType = std::int64_t> std::optional<NumberType> make_a_number(std::string const& s) {
  NumberType value = 0;
  auto [ptr, ec] = std::from_chars(s.c_str(), s.c_str() + s.size(), value);
  if (ec == std::errc()) {
    return value;
  }
  if (ec == std::errc::invalid_argument) {
    std::cerr << s << " is not a valid number\n";
  } else if (ec == std::errc::result_out_of_range) {
    std::cerr << s << " is out of range\n";
  }
  return std::nullopt;
}

std::variant<ast_t, parse_error_t> parser_t::parse() {
  if (m_tokens.empty()) {
    return parse_error_empty_file_t{};
  }

  if (auto pe = parse_program(); has_parse_error(pe)) {
    return pe;
  }
  return ast_t{std::move(m_top_env)};
};

std::optional<const lexer::token_t> parser_t::cur_token() const noexcept {
  if (m_cur_pos == m_tokens.size()) {
    return std::nullopt;
  }
  return m_tokens[m_cur_pos];
}

bool parser_t::try_with(lexer::symbol_t s) const noexcept {
  auto const ct = cur_token();
  return ct && ct->symbol == s;
}

std::optional<const lexer::symbol_t> parser_t::try_with_any_of(std::initializer_list<lexer::symbol_t> ss) {
  auto const p = [this](lexer::symbol_t s) {
    const auto ct = cur_token();
    return ct && ct->symbol == s;
  };
  if (auto const* const it = std::find_if(ss.begin(), ss.end(), p); it != ss.end()) {
    return *it;
  }
  return std::nullopt;
}

parse_error_t parser_t::must_be_any_of(std::initializer_list<lexer::symbol_t> ss) {
  if (try_with_any_of(ss)) {
    return parse_error_ok_t{};
  }

  auto const ct = cur_token();
  if (ct.has_value()) {
    return parse_error_unexpected_t{.expected = {ss}, .got = *ct};
  }
  return parse_error_early_eof_t{.expected = {ss}};
}

parse_error_t parser_t::must_be(lexer::symbol_t s) const noexcept {
  if (try_with(s)) {
    return parse_error_ok_t{};
  }

  auto const ct = cur_token();
  if (ct.has_value()) {
    return parse_error_unexpected_t{.expected = {s}, .got = *ct};
  }
  return parse_error_early_eof_t{.expected = {s}};
}

std::optional<lexer::token_t> parser_t::lookup_name(environment_t const& env, lexer::token_t const& name) {
  auto const find_name = [&name](auto const& container) -> std::optional<lexer::token_t> {
    if (auto const it = std::find_if(container.cbegin(), container.cend(),
                                     [&name](auto const& e) { return sv(name) == sv(e.token()); });
        it != container.cend()) {
      return it->token();
    }
    return std::nullopt;
  };

  for (auto const* cur_env = &env; cur_env != nullptr; cur_env = cur_env->parent) {
    if (auto const t = find_name(cur_env->consts); t.has_value()) {
      return *t;
    }
    if (auto const t = find_name(cur_env->vars); t.has_value()) {
      return *t;
    }
    if (cur_env->parent != nullptr && cur_env->ident == name) {
      return cur_env->ident;
    }
    if (auto const it = std::find_if(cur_env->procedures.cbegin(), cur_env->procedures.cend(),
                                     [&name](auto const& p) { return sv(p.token()) == sv(name); });
        it != cur_env->procedures.cend()) {
      return it->token();
    }
  }
  return std::nullopt;
}

parse_error_t parser_t::parse_program() {
  if (auto pe = parse_block(m_top_env); has_parse_error(pe)) {
    return pe;
  }
  return must_be(lexer::symbol_t::period);
}

parse_error_t parser_t::parse_block(environment_t& env) {
  if (auto pe = parse_consts(env); has_parse_error(pe)) {
    return pe;
  }
  if (auto pe = parse_vars(env); has_parse_error(pe)) {
    return pe;
  }
  if (auto pe = parse_procedures(env); has_parse_error(pe)) {
    return pe;
  }
  auto ret = parse_statements(env);
  if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
    return *pe;
  }
  env.statements = std::move(std::get<std::vector<std::unique_ptr<const statement_t>>>(ret));
  return parse_error_ok_t{};
}

parse_error_t parser_t::parse_consts(environment_t& env) {
  if (!try_with(lexer::symbol_t::const_)) {
    return parse_error_ok_t{};
  }

  while (true) {
    next();
    if (auto pe = must_be(lexer::symbol_t::ident); has_parse_error(pe)) {
      return pe;
    }
    auto const id = *cur_token();
    if (auto const prev_define = lookup_name(env, id); prev_define.has_value()) {
      return parse_error_name_redefined_t{.pre_defined = *prev_define, .cur_defined = id};
    }

    next();
    if (auto pe = must_be(lexer::symbol_t::equal); has_parse_error(pe)) {
      return pe;
    }

    next();
    if (auto pe = must_be(lexer::symbol_t::number); has_parse_error(pe)) {
      return pe;
    }
    auto const num = *cur_token();

    env.consts.emplace_back(id, num);

    next();
    if (try_with(lexer::symbol_t::semicolon)) {
      next();
      if (!try_with(lexer::symbol_t::const_)) {
        return parse_error_ok_t{};
      }
    } else {
      if (auto pe = must_be(lexer::symbol_t::comma); has_parse_error(pe)) {
        return pe;
      }
    }
  }

  return parse_error_ok_t{};
}

parse_error_t parser_t::parse_vars(environment_t& env) {
  if (!try_with(lexer::symbol_t::var)) {
    return parse_error_ok_t{};
  }

  while (true) {
    next();
    if (auto pe = must_be(lexer::symbol_t::ident); has_parse_error(pe)) {
      return pe;
    }
    auto const id = *cur_token();
    if (auto const prev_define = lookup_name(env, id); prev_define.has_value()) {
      return parse_error_name_redefined_t{.pre_defined = *prev_define, .cur_defined = id};
    }

    env.vars.emplace_back(id);

    next();
    if (try_with(lexer::symbol_t::semicolon)) {
      next();
      if (!try_with(lexer::symbol_t::var)) {
        return parse_error_ok_t{};
      }
    } else {
      if (auto pe = must_be(lexer::symbol_t::comma); has_parse_error(pe)) {
        return pe;
      }
    }
  }

  return parse_error_ok_t{};
}

parse_error_t parser_t::parse_procedures(environment_t& env) {
  while (try_with(lexer::symbol_t::proc)) {
    next();
    if (auto pe = must_be(lexer::symbol_t::ident); has_parse_error(pe)) {
      return pe;
    }
    auto const id = *cur_token();
    if (auto const prev_define = lookup_name(env, id); prev_define.has_value()) {
      return parse_error_name_redefined_t{.pre_defined = *prev_define, .cur_defined = id};
    }
    next();

    if (auto pe = must_be(lexer::symbol_t::semicolon); has_parse_error(pe)) {
      return pe;
    }
    next();

    environment_t block_env{};
    block_env.parent = &env;
    block_env.ident = id;
    if (auto pe = parse_block(block_env); has_parse_error(pe)) {
      return pe;
    }

    if (auto pe = must_be(lexer::symbol_t::semicolon); has_parse_error(pe)) {
      return pe;
    }
    next();

    env.procedures.emplace_back(id, std::move(block_env));
  }
  return parse_error_ok_t{};
}

parser_t::result_t<in_t> parser_t::parse_in(environment_t const& env) {
  next();

  if (auto pe = must_be(lexer::symbol_t::ident); has_parse_error(pe)) {
    return pe;
  }
  auto const id = *cur_token();
  if (auto const prev_define = lookup_name(env, id); !prev_define.has_value()) {
    return parse_error_name_undefined_t{.name = id};
  }

  next();

  return in_t{id};
}

parser_t::result_t<out_t> parser_t::parse_out(environment_t const& env) {
  next();

  auto ret = parse_expression(env);
  if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
    return *pe;
  }
  return out_t{std::move(std::get<std::unique_ptr<const expression_t>>(ret))};
}

parser_t::result_t<call_t> parser_t::parse_call(environment_t const& /*env*/) {
  next();

  if (auto pe = must_be(lexer::symbol_t::ident); has_parse_error(pe)) {
    return pe;
  }
  auto const id = *cur_token();

  next();

  return call_t{id};
}

parser_t::result_t<becomes_t> parser_t::parse_becomes(environment_t const& env) {
  auto const id = *cur_token();
  if (auto const prev_define = lookup_name(env, id); !prev_define.has_value()) {
    return parse_error_name_undefined_t{.name = id};
  }
  next();

  if (auto pe = must_be(lexer::symbol_t::becomes); has_parse_error(pe)) {
    return pe;
  }
  next();

  auto expr = parse_expression(env);
  if (auto const* pe = std::get_if<parse_error_t>(&expr); pe != nullptr) {
    return *pe;
  }

  return becomes_t{id, std::move(std::get<std::unique_ptr<const expression_t>>(expr))};
}

parser_t::result_t<begin_end_t> parser_t::parse_begin_end(environment_t const& env) {
  std::vector<std::unique_ptr<const statement_t>> statements;
  std::vector<std::unique_ptr<const statement_t>> sub_statements;
  do {
    next();
    auto ret = parse_statements(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    sub_statements = std::move(std::get<std::vector<std::unique_ptr<const statement_t>>>(ret));
    statements.insert(statements.end(), std::make_move_iterator(sub_statements.begin()),
                      std::make_move_iterator(sub_statements.end()));
  } while (try_with(lexer::symbol_t::semicolon));
  if (auto pe = must_be(lexer::symbol_t::end); has_parse_error(pe)) {
    return pe;
  }
  next();

  return begin_end_t{std::move(statements)};
}

parser_t::ptr_result_t<condition_t> parser_t::parse_condition(environment_t const& env) {
  next();
  if (try_with(lexer::symbol_t::odd)) {
    next();
    auto ret = parse_expression(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    return std::make_unique<const odd_condition_t>(std::move(std::get<std::unique_ptr<const expression_t>>(ret)));
  }
  auto ret = parse_expression(env);
  if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
    return *pe;
  }
  auto lhs = std::move(std::get<std::unique_ptr<const expression_t>>(ret));

  if (auto pe = must_be_any_of({lexer::symbol_t::equal, lexer::symbol_t::not_equal, lexer::symbol_t::less_equal,
                                lexer::symbol_t::less, lexer::symbol_t::greater_equal, lexer::symbol_t::greater});
      has_parse_error(pe)) {
    return pe;
  }
  auto const op = *cur_token();
  next();

  ret = parse_expression(env);
  if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
    return *pe;
  }
  auto rhs = std::move(std::get<std::unique_ptr<const expression_t>>(ret));

  return std::make_unique<const cmp_condition_t>(op, std::move(lhs), std::move(rhs));
}

parser_t::ptr_result_t<statement_t> parser_t::parse_statement(environment_t const& env) {
  if (try_with(lexer::symbol_t::in)) {
    auto ret = parse_in(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    return std::make_unique<const in_t>(std::move(std::get<in_t>(ret)));
  }
  if (try_with(lexer::symbol_t::call)) {
    auto ret = parse_call(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    return std::make_unique<const call_t>(std::move(std::get<call_t>(ret)));
  }
  if (try_with(lexer::symbol_t::out)) {
    auto ret = parse_out(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    return std::make_unique<const out_t>(std::move(std::get<out_t>(ret)));
  }
  if (try_with(lexer::symbol_t::ident)) {
    auto ret = parse_becomes(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    return std::make_unique<const becomes_t>(std::move(std::get<becomes_t>(ret)));
  }
  if (try_with(lexer::symbol_t::begin)) {
    auto ret = parse_begin_end(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    return std::make_unique<const begin_end_t>(std::move(std::get<begin_end_t>(ret)));
  }
  if (try_with(lexer::symbol_t::if_)) {
    auto ret = parse_if_then(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    return std::make_unique<const if_then_t>(std::move(std::get<if_then_t>(ret)));
  }
  if (try_with(lexer::symbol_t::while_)) {
    auto ret = parse_while_do(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    return std::make_unique<const while_do_t>(std::move(std::get<while_do_t>(ret)));
  }
  __builtin_unreachable();
}

parser_t::result_t<if_then_t> parser_t::parse_if_then(environment_t const& env) {
  auto condition = parse_condition(env);
  if (auto const* pe = std::get_if<parse_error_t>(&condition); pe != nullptr) {
    return *pe;
  }

  if (auto const pe = must_be(lexer::symbol_t::then); has_parse_error(pe)) {
    return pe;
  }
  next();

  auto statement = parse_statement(env);
  if (auto const* pe = std::get_if<parse_error_t>(&statement); pe != nullptr) {
    return *pe;
  }

  return if_then_t{std::move(std::get<std::unique_ptr<const condition_t>>(condition)),
                   std::move(std::get<std::unique_ptr<const statement_t>>(statement))};
}

parser_t::result_t<while_do_t> parser_t::parse_while_do(environment_t const& env) {
  auto condition = parse_condition(env);
  if (auto const* pe = std::get_if<parse_error_t>(&condition); pe != nullptr) {
    return *pe;
  }

  if (auto const pe = must_be(lexer::symbol_t::do_); has_parse_error(pe)) {
    return pe;
  }
  next();

  auto statement = parse_statement(env);
  if (auto const* pe = std::get_if<parse_error_t>(&statement); pe != nullptr) {
    return *pe;
  }

  return while_do_t{std::move(std::get<std::unique_ptr<const condition_t>>(condition)),
                    std::move(std::get<std::unique_ptr<const statement_t>>(statement))};
}

parser_t::ptr_vec_result_t<statement_t> parser_t::parse_statements(environment_t const& env) {
  std::vector<std::unique_ptr<const statement_t>> statements;
  while (true) {
    if (!try_with_any_of({lexer::symbol_t::in, lexer::symbol_t::call, lexer::symbol_t::out, lexer::symbol_t::ident,
                          lexer::symbol_t::begin, lexer::symbol_t::if_, lexer::symbol_t::while_})
             .has_value()) {
      break;
    }
    auto ret = parse_statement(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    statements.push_back(std::move(std::get<std::unique_ptr<const statement_t>>(ret)));
  }
  return statements;
}

parser_t::ptr_result_t<expression_t> parser_t::parse_expression(environment_t const& env) {
  std::vector<std::unique_ptr<const expression_t>> expressions{};
  auto ret = parse_expression_primary(env);
  if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
    return *pe;
  }
  expressions.push_back(std::move(std::get<std::unique_ptr<const expression_t>>(ret)));
  std::vector<lexer::token_t> ops{};
  return parse_expression_precedence_climbing(env, expressions, ops, 0);
}

parser_t::ptr_result_t<expression_t> parser_t::parse_expression_primary(environment_t const& env) {
  if (try_with(lexer::symbol_t::minus)) {
    next();
    auto ret = parse_expression_without_leading_sign(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    return std::make_unique<const expression_binary_op_t>(
        lexer::token_t{.symbol = lexer::symbol_t::times, .annotation = {.source = "*", .start = 0U, .length = 1U}},
        std::make_unique<const number_t>(lexer::token_t{.symbol = lexer::symbol_t::number,
                                                        .annotation = {.source = "-1", .start = 0U, .length = 2}}),
        std::move(std::get<std::unique_ptr<const expression_t>>(ret)));
  }
  if (try_with(lexer::symbol_t::plus)) {
    next();
  }
  return parse_expression_without_leading_sign(env);
}

parser_t::ptr_result_t<expression_t> parser_t::parse_expression_without_leading_sign(environment_t const& env) {
  if (auto pe = must_be_any_of({lexer::symbol_t::number, lexer::symbol_t::ident, lexer::symbol_t::lparen});
      has_parse_error(pe)) {
    return pe;
  }

  if (try_with(lexer::symbol_t::number)) {
    auto ret = std::make_unique<const number_t>(*cur_token());
    next();
    return ret;
  }
  if (try_with(lexer::symbol_t::ident)) {
    auto const id = *cur_token();
    if (auto const prev_define = lookup_name(env, id); !prev_define.has_value()) {
      return parse_error_name_undefined_t{.name = id};
    }
    auto ret = std::make_unique<const ident_t>(id);
    next();
    return ret;
  }
  if (try_with(lexer::symbol_t::lparen)) {
    next();
    auto ret = parse_expression(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    if (auto pe = must_be(lexer::symbol_t::rparen); has_parse_error(pe)) {
      return pe;
    }
    next();
    return std::move(std::get<std::unique_ptr<const expression_t>>(ret));
  }
  __builtin_unreachable();
}

parser_t::ptr_result_t<expression_t>
parser_t::parse_expression_precedence_climbing(environment_t const& env,
                                               std::vector<std::unique_ptr<const expression_t>>& expressions,
                                               std::vector<lexer::token_t>& ops, int precedence) {
  static std::map<lexer::symbol_t, int> const precedences = {
      {lexer::symbol_t::plus, 1},
      {lexer::symbol_t::minus, 1},
      {lexer::symbol_t::times, 2},
      {lexer::symbol_t::divide, 2},
  };

  while (cur_token().has_value()) {
    auto const it = precedences.find(cur_token()->symbol);
    if (it == precedences.cend()) {
      break;
    }

    if (precedence < it->second) {
      ops.push_back(*cur_token());
      next();
      precedence = it->second;
    } else if (precedence == it->second) {
      auto const op = ops.back();
      ops.back() = *cur_token();
      next();
      auto rhs = std::move(expressions.back());
      expressions.pop_back();
      auto lhs = std::move(expressions.back());
      expressions.back() = std::make_unique<const expression_binary_op_t>(op, std::move(lhs), std::move(rhs));
    } else {
      parse_expression_reduce_all(expressions, ops);
      precedence = it->second;
      ops.push_back(*cur_token());
      next();
    }
    auto ret = parse_expression_without_leading_sign(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    expressions.push_back(std::move(std::get<std::unique_ptr<const expression_t>>(ret)));
  }
  parse_expression_reduce_all(expressions, ops);
  return std::move(expressions[0]);
}

void parser_t::parse_expression_reduce_all(std::vector<std::unique_ptr<const expression_t>>& expressions,
                                           std::vector<lexer::token_t>& ops) {
  // NOLINTNEXTLINE(hicpp-no-array-decay, cppcoreguidelines-pro-bounds-array-to-pointer-decay)
  assert(ops.size() + 1 == expressions.size());
  while (!ops.empty()) {
    auto rhs = std::move(expressions.back());
    expressions.pop_back();
    auto lhs = std::move(expressions.back());
    auto const op = ops.back();
    ops.pop_back();
    expressions.back() = std::make_unique<const expression_binary_op_t>(op, std::move(lhs), std::move(rhs));
  }
}
} // namespace parser

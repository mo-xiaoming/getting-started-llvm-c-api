#include "lib.hpp"

#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/Transforms/IPO.h>
#include <llvm-c/Transforms/Scalar.h>
#include <llvm-c/Transforms/Utils.h>

#include <array>
#include <cassert>
#include <cinttypes>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <memory>

namespace {
auto to_int(char const* s) { return std::strtoll(s, nullptr, 10); }

using native_int_t = std::int64_t;

auto make_builder() {
  return std::shared_ptr<std::remove_pointer_t<LLVMBuilderRef>>{LLVMCreateBuilder(), LLVMDisposeBuilder};
}

LLVMValueRef create_fib_fn(LLVMModuleRef mod) {
  LLVMTypeRef llvm_int_ty = LLVMInt64Type();

  LLVMTypeRef proto_ty = LLVMFunctionType(llvm_int_ty, &llvm_int_ty, 1U, /*isVarArg*/ 0);
  LLVMValueRef fn = LLVMAddFunction(mod, "fib", proto_ty);

  LLVMBasicBlockRef bb_entry = LLVMAppendBasicBlock(fn, "entry");
  LLVMBasicBlockRef bb_ret = LLVMAppendBasicBlock(fn, "return");
  LLVMBasicBlockRef bb_tail = LLVMAppendBasicBlock(fn, "tail");

  {
    auto const builder = make_builder();
    // if (n == 0 || n == 1)
    LLVMPositionBuilderAtEnd(builder.get(), bb_entry);
    LLVMValueRef n = LLVMGetFirstParam(fn);
    LLVMSetValueName(n, "n");
    LLVMValueRef cond0 = LLVMBuildICmp(builder.get(), LLVMIntPredicate::LLVMIntEQ,
                                       LLVMConstInt(llvm_int_ty, 0ULL, /*SignedExtend*/ 0), n, "eq0");
    LLVMValueRef cond1 = LLVMBuildICmp(builder.get(), LLVMIntPredicate::LLVMIntEQ,
                                       LLVMConstInt(llvm_int_ty, 1ULL, /*SignedExtend*/ 0), n, "eq1");
    LLVMValueRef cond = LLVMBuildOr(builder.get(), cond0, cond1, "cond");
    LLVMBuildCondBr(builder.get(), cond, bb_ret, bb_tail);

    // then
    // fib(n) == n
    LLVMPositionBuilderAtEnd(builder.get(), bb_ret);
    LLVMBuildRet(builder.get(), n);

    // else
    // fib(n-1) + fib(n-2)
    LLVMPositionBuilderAtEnd(builder.get(), bb_tail);
    LLVMValueRef n1 = LLVMBuildSub(builder.get(), n, LLVMConstInt(llvm_int_ty, 1ULL, /*SignedExtend*/ 0), "n1");
    LLVMValueRef f1 = LLVMBuildCall2(builder.get(), proto_ty, fn, &n1, 1U, "f1");
    LLVMSetTailCall(f1, /*IsTailCall*/ 1); // Doesn't seem to make any difference
    LLVMValueRef n2 = LLVMBuildSub(builder.get(), n, LLVMConstInt(llvm_int_ty, 2ULL, /*SignedExtend*/ 0), "n2");
    LLVMValueRef f2 = LLVMBuildCall2(builder.get(), proto_ty, fn, &n2, 1U, "f2");
    LLVMSetTailCall(f2, /*IsTailCall*/ 1); // Doesn't seem to make any difference
    LLVMValueRef ret = LLVMBuildAdd(builder.get(), f1, f2, "ret");
    LLVMBuildRet(builder.get(), ret);
  }

  LLVMVerifyFunction(fn, LLVMAbortProcessAction);
  return fn;
}

native_int_t exec_fib_fn(uint64_t addr, native_int_t n) {             // NOLINT
  auto func = reinterpret_cast<native_int_t (*)(native_int_t)>(addr); // NOLINT
  return func(n);
}

LLVMValueRef create_factorial_fn(LLVMModuleRef mod) {
  LLVMTypeRef llvm_int_ty = LLVMInt64Type();

  LLVMTypeRef proto_ty = LLVMFunctionType(llvm_int_ty, &llvm_int_ty, 1U, /*isVarArg*/ 0);
  LLVMValueRef fn = LLVMAddFunction(mod, "factorial", proto_ty);

  LLVMBasicBlockRef bb_entry = LLVMAppendBasicBlock(fn, "entry");
  LLVMBasicBlockRef bb_ret = LLVMAppendBasicBlock(fn, "return");
  LLVMBasicBlockRef bb_tail = LLVMAppendBasicBlock(fn, "tail");

  {
    auto const builder = make_builder();
    // if (n == 0)
    LLVMPositionBuilderAtEnd(builder.get(), bb_entry);
    LLVMValueRef n = LLVMGetFirstParam(fn);
    LLVMSetValueName(n, "n");
    LLVMValueRef cond = LLVMBuildICmp(builder.get(), LLVMIntPredicate::LLVMIntEQ,
                                      LLVMConstInt(llvm_int_ty, 0ULL, /*SignedExtend*/ 0), n, "cond");
    LLVMBuildCondBr(builder.get(), cond, bb_ret, bb_tail);

    // then
    // !0 == 1
    LLVMPositionBuilderAtEnd(builder.get(), bb_ret);
    LLVMBuildRet(builder.get(), LLVMConstInt(llvm_int_ty, 1ULL, /*SignExtend*/ 0));

    // else
    // n * factorial(n-1)
    LLVMPositionBuilderAtEnd(builder.get(), bb_tail);
    LLVMValueRef n1 = LLVMBuildSub(builder.get(), n, LLVMConstInt(llvm_int_ty, 1ULL, /*SignedExtend*/ 0), "n1");
    LLVMValueRef f1 = LLVMBuildCall2(builder.get(), proto_ty, fn, &n1, 1U, "f1");
    LLVMSetTailCall(f1, /*IsTailCall*/ 1); // Doesn't seem to make any difference
    LLVMValueRef ret = LLVMBuildMul(builder.get(), n, f1, "ret");
    LLVMBuildRet(builder.get(), ret);
  }

  LLVMVerifyFunction(fn, LLVMAbortProcessAction);
  return fn;
}

native_int_t exec_factorial_fn(uint64_t addr, native_int_t n) {       // NOLINT
  auto func = reinterpret_cast<native_int_t (*)(native_int_t)>(addr); // NOLINT
  return func(n);
}

/**
 * int sum(int a, int b) {
 *     return a + b;
 * }
 */
LLVMValueRef create_sum_fn(LLVMModuleRef mod) {
  LLVMTypeRef llvm_int_ty = LLVMInt64Type();

  auto param_types = std::array{llvm_int_ty, llvm_int_ty};
  LLVMTypeRef proto_ty = LLVMFunctionType(llvm_int_ty, param_types.data(), 2U, /*isVarArg*/ 0);
  LLVMValueRef sum_fn = LLVMAddFunction(mod, "sum", proto_ty);

  LLVMBasicBlockRef entry = LLVMAppendBasicBlock(sum_fn, "entry");

  {
    auto const builder = make_builder();
    LLVMPositionBuilderAtEnd(builder.get(), entry);
    LLVMValueRef a = LLVMGetParam(sum_fn, 0);
    LLVMSetValueName(a, "a");
    LLVMValueRef b = LLVMGetParam(sum_fn, 1);
    LLVMSetValueName(b, "b");
    LLVMValueRef add_tmp = LLVMBuildAdd(builder.get(), a, b, "add_tmp");
    LLVMBuildRet(builder.get(), add_tmp);
  }

  LLVMVerifyFunction(sum_fn, LLVMAbortProcessAction);
  return sum_fn;
}

auto exec_sum_fn(uint64_t addr, char const* a, char const* b) {
  auto func = reinterpret_cast<native_int_t (*)(native_int_t, native_int_t)>(addr); // NOLINT
  native_int_t x = to_int(a);
  native_int_t y = to_int(b);
  return func(x, y);
}

struct extern_fn_t {
  LLVMTypeRef proto_ty;
  LLVMValueRef fn;
};

/**
 * int puts(const char* s);
 */
extern_fn_t declare_puts_fn(LLVMModuleRef mod) {
  LLVMTypeRef char_ptr_ty = LLVMPointerType(LLVMInt8Type(), /*AddressSpace*/ 0);
  auto param_types = std::array{char_ptr_ty};
  LLVMTypeRef proto_ty = LLVMFunctionType(LLVMInt32Type(), param_types.data(), 1U, /*isVarArg*/ 0);
  LLVMValueRef puts_fn = LLVMAddFunction(mod, "puts", proto_ty);

  return {.proto_ty = proto_ty, .fn = puts_fn};
}

/**
 * int out(const char* s)
 */
LLVMValueRef create_out_fn(LLVMModuleRef mod) {
  auto [puts_proto_ty, puts_fn] = declare_puts_fn(mod);

  LLVMValueRef out_fn = LLVMAddFunction(mod, "out", puts_proto_ty);
  LLVMBasicBlockRef entry = LLVMAppendBasicBlock(out_fn, "entry");

  {
    auto const builder = make_builder();
    LLVMPositionBuilderAtEnd(builder.get(), entry);
    LLVMValueRef str = LLVMGetFirstParam(out_fn);
    LLVMSetValueName(str, "str");
    LLVMValueRef cnt = LLVMBuildCall2(builder.get(), puts_proto_ty, puts_fn, &str, 1, "ch_cnt");
    LLVMBuildRet(builder.get(), cnt);
  }

  LLVMVerifyFunction(out_fn, LLVMAbortProcessAction);

  return out_fn;
}

auto exec_out_fn(uint64_t addr, char const* s) {
  auto func = reinterpret_cast<int (*)(char const*)>(addr); // NOLINT
  return func(s);
}

constexpr auto Fn_Opt_Cnt = 1;
constexpr auto Module_Opt_Cnt = 1;

struct fn_pass_manager_t {
  explicit fn_pass_manager_t(LLVMModuleRef mod) : m_fpm{LLVMCreateFunctionPassManagerForModule(mod)} {
    LLVMAddBasicAliasAnalysisPass(m_fpm);
    LLVMAddPromoteMemoryToRegisterPass(m_fpm);
    LLVMAddInstructionCombiningPass(m_fpm);
    LLVMAddInstructionSimplifyPass(m_fpm);
    LLVMAddReassociatePass(m_fpm);
    LLVMAddGVNPass(m_fpm);
    LLVMAddCFGSimplificationPass(m_fpm);
    LLVMAddTailCallEliminationPass(m_fpm);
    LLVMInitializeFunctionPassManager(m_fpm);
  }
  fn_pass_manager_t(fn_pass_manager_t const&) = delete;
  fn_pass_manager_t(fn_pass_manager_t&&) = delete;
  fn_pass_manager_t& operator=(fn_pass_manager_t const&) = delete;
  fn_pass_manager_t& operator=(fn_pass_manager_t&&) = delete;
  ~fn_pass_manager_t() { LLVMDisposePassManager(m_fpm); }

  void operator()(LLVMValueRef fn, char const* name) {
    std::cerr << "---------- function `" << name << "` before optimization\n";
    LLVMDumpValue(fn);
    for (auto i = 0; i < Fn_Opt_Cnt; ++i) {
      if (LLVMRunFunctionPassManager(m_fpm, fn) == 0) {
        break;
      }
      std::cerr << "---------- function `" << name << "` after iteration " << i << '\n';
      LLVMDumpValue(fn);
    }
  }

private:
  LLVMPassManagerRef m_fpm;
};

void opt_on_module(LLVMModuleRef mod) {
  auto* mpm = LLVMCreatePassManager();
  LLVMAddFunctionInliningPass(mpm);
  LLVMAddMergeFunctionsPass(mpm);
  std::cerr << "---------- module before optimization\n";
  LLVMDumpModule(mod);
  for (auto i = 0; i < Module_Opt_Cnt; ++i) {
    if (LLVMRunPassManager(mpm, mod) == 0) {
      break;
    }
    std::cerr << "---------- after iteration " << i << '\n';
    LLVMDumpModule(mod);
  }
  LLVMDisposePassManager(mpm);
}

struct jit_t {
  explicit jit_t(LLVMModuleRef mod) : m_module{mod} {
    LLVMLinkInMCJIT();
    LLVMInitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();

    opt_on_module(m_module);

    char* error = nullptr;
    if (LLVMCreateExecutionEngineForModule(&m_engine, m_module, &error) != 0) {
      std::cerr << "failed to create execution engine, " << error << '\n';
      LLVMDisposeMessage(error);
      assert(false); // NOLINT
    }
  }
  jit_t(jit_t const&) = delete;
  jit_t(jit_t&&) = delete;
  jit_t& operator=(jit_t const&) = delete;
  jit_t& operator=(jit_t&&) = delete;
  ~jit_t() { LLVMDisposeExecutionEngine(m_engine); }

  uint64_t fn_addr(char const* name) { return LLVMGetFunctionAddress(m_engine, name); }

private:
  LLVMModuleRef m_module;
  LLVMExecutionEngineRef m_engine = nullptr;
};

void verify_module(LLVMModuleRef mod) {
  char* error = nullptr;
  LLVMVerifyModule(mod, LLVMAbortProcessAction, &error);
  LLVMDisposeMessage(error);
}
} // namespace

namespace lib {
void all() {
  LLVMModuleRef mod = LLVMModuleCreateWithName("jit-mod");

  fn_pass_manager_t fpm{mod};

  char const* sum_name = "sum";
  auto* const sum_fn = create_sum_fn(mod);
  fpm(sum_fn, sum_name);

  char const* out_name = "out";
  auto* const out_fn = create_out_fn(mod);
  fpm(out_fn, out_name);

  char const* fact_name = "factorial";
  auto* const fact_fn = create_factorial_fn(mod);
  fpm(fact_fn, fact_name);

  char const* fib_name = "fib";
  auto* const fib_fn = create_fib_fn(mod);
  fpm(fib_fn, fib_name);

  verify_module(mod);

  jit_t jit{mod};

  std::cerr << "----------------------------------------------------\n";

  auto sum_addr = jit.fn_addr(sum_name);
  assert(exec_sum_fn(sum_addr, "29", "13") == 42LL); // NOLINT

  auto out_addr = jit.fn_addr(out_name);
  exec_out_fn(out_addr, "hello, out");

  auto fact_addr = jit.fn_addr(fact_name);
  assert(exec_factorial_fn(fact_addr, 4) == 24LL); // NOLINT

  auto fib_addr = jit.fn_addr(fib_name);
  assert(exec_fib_fn(fib_addr, 14) == 377LL); // NOLINT
}
} // namespace lib

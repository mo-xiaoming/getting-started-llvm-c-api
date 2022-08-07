// My question to Lang Hames about what does thread-safe mean in ThreadSafeContext and ThreadSafeModule
//
// https://groups.google.com/g/llvm-dev/c/QauU4L_bHac
//
// > The only scenario where locking becomes relevant is when you have multiple threads and a single ThreadSafeContext
// > shared between two or more ThreadSafeModules
//
// > My recommendation is to always start with one context per module, and only share contexts if you see memory
// > consumption from the contexts becoming an issue.

#include "orcv2jit.hpp"

#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/Core.h>
#include <llvm-c/LLJIT.h>
#include <llvm-c/Orc.h>
#include <llvm-c/Target.h>
#include <llvm-c/Transforms/IPO.h>
#include <llvm-c/Transforms/Scalar.h>
#include <llvm-c/Transforms/Utils.h>

#include <array>
#include <atomic>
#include <cassert>
#include <cinttypes>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <thread>
#include <vector>

namespace {
using native_int_t = std::int64_t;

auto make_builder(LLVMContextRef ctx) {
  return std::shared_ptr<std::remove_pointer_t<LLVMBuilderRef>>{LLVMCreateBuilderInContext(ctx), LLVMDisposeBuilder};
}

auto make_tsc() {
  return std::shared_ptr<std::remove_pointer_t<LLVMOrcThreadSafeContextRef>>{LLVMOrcCreateNewThreadSafeContext(),
                                                                             LLVMOrcDisposeThreadSafeContext};
}

auto make_mpm() {
  return std::shared_ptr<std::remove_pointer_t<LLVMPassManagerRef>>{LLVMCreatePassManager(), LLVMDisposePassManager};
}

auto make_fpm(LLVMModuleRef mod) {
  return std::shared_ptr<std::remove_pointer_t<LLVMPassManagerRef>>{LLVMCreateFunctionPassManagerForModule(mod),
                                                                    LLVMDisposePassManager};
}

bool has_function_defined(LLVMValueRef fn) { return LLVMCountBasicBlocks(fn) != 0; }

struct extern_fn_t {
  LLVMTypeRef proto_ty;
  LLVMValueRef fn;
};

extern "C" void say_hello() { std::cout << "say hello!" << std::endl; }

/**
 * int say_hello();
 */
extern_fn_t declare_say_hello_fn(LLVMModuleRef mod) {
  LLVMContextRef ctx = LLVMGetModuleContext(mod);

  LLVMTypeRef proto_ty = LLVMFunctionType(LLVMVoidTypeInContext(ctx), nullptr, 0U, /*isVarArg*/ 0);
  LLVMValueRef fn = LLVMAddFunction(mod, "say_hello", proto_ty);

  return {.proto_ty = proto_ty, .fn = fn};
}

/**
 * int64 factorial(int64 n) {
 *   if (n == 0) return 1;
 *   return n * factorial(n-1);
 * }
 */
extern_fn_t declare_factorial_fn(LLVMModuleRef mod) {
  LLVMContextRef ctx = LLVMGetModuleContext(mod);

  LLVMTypeRef llvm_int_ty = LLVMInt64TypeInContext(ctx);

  LLVMTypeRef proto_ty = LLVMFunctionType(llvm_int_ty, &llvm_int_ty, 1U, /*isVarArg*/ 0);
  LLVMValueRef fn = LLVMAddFunction(mod, "factorial", proto_ty);

  return {.proto_ty = proto_ty, .fn = fn};
}

LLVMValueRef create_factorial_fn(LLVMModuleRef mod) {
  LLVMContextRef ctx = LLVMGetModuleContext(mod);

  LLVMTypeRef llvm_int_ty = LLVMInt64TypeInContext(ctx);

  auto [proto_ty, fn] = declare_factorial_fn(mod);

  LLVMValueRef Zero = LLVMConstInt(llvm_int_ty, 0ULL, /*SignExtend*/ 0);
  LLVMValueRef One = LLVMConstInt(llvm_int_ty, 1ULL, /*SignExtend*/ 0);

  LLVMBasicBlockRef bb_entry = LLVMAppendBasicBlockInContext(ctx, fn, "entry");
  LLVMBasicBlockRef bb_ret = LLVMAppendBasicBlockInContext(ctx, fn, "return");
  LLVMBasicBlockRef bb_tail = LLVMAppendBasicBlockInContext(ctx, fn, "tail");

  {
    auto const builder = make_builder(ctx);
    // if (n == 0)
    LLVMPositionBuilderAtEnd(builder.get(), bb_entry);
    LLVMValueRef n = LLVMGetFirstParam(fn);
    LLVMSetValueName(n, "n");
    LLVMValueRef cond = LLVMBuildICmp(builder.get(), LLVMIntPredicate::LLVMIntEQ, Zero, n, "cond");
    LLVMBuildCondBr(builder.get(), cond, bb_ret, bb_tail);

    // then
    // !0 == 1
    LLVMPositionBuilderAtEnd(builder.get(), bb_ret);
    LLVMBuildRet(builder.get(), One);

    // else
    // n * factorial(n-1)
    LLVMPositionBuilderAtEnd(builder.get(), bb_tail);
    LLVMValueRef n1 = LLVMBuildSub(builder.get(), n, One, "n1");
    LLVMValueRef f1 = LLVMBuildCall2(builder.get(), proto_ty, fn, &n1, 1U, "f1");
    LLVMSetTailCall(f1, /*IsTailCall*/ 1); // Doesn't seem to make any difference
    LLVMValueRef ret = LLVMBuildMul(builder.get(), n, f1, "ret");
    LLVMBuildRet(builder.get(), ret);
  }

  LLVMVerifyFunction(fn, LLVMAbortProcessAction);
  return fn;
}

/**
 * int puts(const char* s);
 */
extern_fn_t declare_puts_fn(LLVMModuleRef mod) {
  LLVMContextRef ctx = LLVMGetModuleContext(mod);

  LLVMTypeRef char_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(ctx), /*AddressSpace*/ 0);
  LLVMTypeRef proto_ty = LLVMFunctionType(LLVMInt32TypeInContext(ctx), &char_ptr_ty, 1U, /*isVarArg*/ 0);
  LLVMValueRef puts_fn = LLVMAddFunction(mod, "puts", proto_ty);

  return {.proto_ty = proto_ty, .fn = puts_fn};
}

/**
 * int out(const char* s)
 */
extern_fn_t declare_out_fn(LLVMModuleRef mod) {
  LLVMContextRef ctx = LLVMGetModuleContext(mod);

  LLVMTypeRef char_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(ctx), /*AddressSpace*/ 0);
  LLVMTypeRef proto_ty = LLVMFunctionType(LLVMInt32TypeInContext(ctx), &char_ptr_ty, 1U, /*isVarArg*/ 0);
  LLVMValueRef fn = LLVMAddFunction(mod, "out", proto_ty);

  return {.proto_ty = proto_ty, .fn = fn};
}

LLVMValueRef create_out_fn(LLVMModuleRef mod) {
  LLVMContextRef ctx = LLVMGetModuleContext(mod);

  auto [puts_proto_ty, puts_fn] = declare_puts_fn(mod);

  auto [out_proto_ty, out_fn] = declare_out_fn(mod);

  LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(ctx, out_fn, "entry");

  {
    auto const builder = make_builder(ctx);
    LLVMPositionBuilderAtEnd(builder.get(), entry);
    LLVMValueRef str = LLVMGetFirstParam(out_fn);
    LLVMSetValueName(str, "str");
    LLVMValueRef cnt = LLVMBuildCall2(builder.get(), puts_proto_ty, puts_fn, &str, 1U, "ch_cnt");
    LLVMBuildRet(builder.get(), cnt);
  }

  LLVMVerifyFunction(out_fn, LLVMAbortProcessAction);

  return out_fn;
}

/**
 * void cal_fact_4()
 */
LLVMValueRef create_cal_fact_4_fn(LLVMModuleRef mod) {
  LLVMContextRef ctx = LLVMGetModuleContext(mod);

  LLVMTypeRef fact_int_ty = LLVMInt64TypeInContext(ctx);

  LLVMTypeRef proto_ty = LLVMFunctionType(LLVMVoidTypeInContext(ctx), nullptr, 0U, /*isVarArg*/ 0);
  LLVMValueRef fn = LLVMAddFunction(mod, "cal_fact_4", proto_ty);

  LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(ctx, fn, "entry");
  LLVMBasicBlockRef bb_wrong = LLVMAppendBasicBlockInContext(ctx, fn, "wrong");
  LLVMBasicBlockRef bb_return = LLVMAppendBasicBlockInContext(ctx, fn, "return");

  {
    auto const builder = make_builder(ctx);
    LLVMPositionBuilderAtEnd(builder.get(), entry);
    auto [say_hello_proto_ty, say_hello_fn] = declare_say_hello_fn(mod);
    LLVMBuildCall2(builder.get(), say_hello_proto_ty, say_hello_fn, nullptr, 0U, "");

    LLVMValueRef correct_str = LLVMBuildGlobalStringPtr(builder.get(), "!4 == 24", "correct_str");
    LLVMValueRef wrong_str = LLVMBuildGlobalStringPtr(builder.get(), "shit! !4 != 24", "wrong_str");
    LLVMTypeRef char_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(ctx), /*AddressSpace*/ 0);
    LLVMValueRef str_addr = LLVMBuildAlloca(builder.get(), char_ptr_ty, "str");
    LLVMBuildStore(builder.get(), correct_str, str_addr);

    // fact_4 = factorial(4)
    auto [fact_proto_ty, fact_fn] = declare_factorial_fn(mod);
    LLVMValueRef fact_arg0 = LLVMConstInt(fact_int_ty, 4ULL, /*SignExtend*/ 0);
    LLVMValueRef fact_4 = LLVMBuildCall2(builder.get(), fact_proto_ty, fact_fn, &fact_arg0, 1U, "fact_4");

    // if (fact_4 == 24)
    LLVMValueRef cond = LLVMBuildICmp(builder.get(), LLVMIntPredicate::LLVMIntEQ, fact_4,
                                      LLVMConstInt(fact_int_ty, 24ULL, /*SignExtend*/ 0), "cond");
    LLVMBuildCondBr(builder.get(), cond, bb_return, bb_wrong);

    // wrong
    LLVMPositionBuilderAtEnd(builder.get(), bb_wrong);
    LLVMBuildStore(builder.get(), wrong_str, str_addr);
    LLVMBuildBr(builder.get(), bb_return);

    // return
    LLVMPositionBuilderAtEnd(builder.get(), bb_return);
    auto [out_proto_ty, out_fn] = declare_out_fn(mod);
    LLVMValueRef str = LLVMBuildLoad2(builder.get(), char_ptr_ty, str_addr, "");
    LLVMBuildCall2(builder.get(), out_proto_ty, out_fn, &str, 1U, "");
    LLVMBuildRetVoid(builder.get());
  }

  LLVMVerifyFunction(fn, LLVMAbortProcessAction);
  return fn;
}

auto exec_cal_fact_4_fn(uint64_t addr) {
  auto func = reinterpret_cast<void (*)()>(addr); // NOLINT
  return func();
}

struct fn_pass_manager_t {
  explicit fn_pass_manager_t(LLVMModuleRef mod) : m_mod{mod}, m_fpm{make_fpm(m_mod)} {
    LLVMAddBasicAliasAnalysisPass(m_fpm.get());
    LLVMAddPromoteMemoryToRegisterPass(m_fpm.get());
    LLVMAddInstructionCombiningPass(m_fpm.get());
    LLVMAddInstructionSimplifyPass(m_fpm.get());
    LLVMAddReassociatePass(m_fpm.get());
    LLVMAddNewGVNPass(m_fpm.get());
    LLVMAddCFGSimplificationPass(m_fpm.get());
    LLVMAddTailCallEliminationPass(m_fpm.get());
  }

  void operator()(LLVMValueRef fn, char const* name) {
    std::cerr << "---------- function `" << name << "` before optimization\n";
    LLVMDumpValue(fn);
    if (LLVMRunFunctionPassManager(m_fpm.get(), fn) != 0) {
      std::cerr << "---------- function `" << name << "` after optimization" << '\n';
      LLVMDumpValue(fn);
    }
  }

  void operator()() {
    for (LLVMValueRef fn = LLVMGetFirstFunction(m_mod); fn != nullptr; fn = LLVMGetNextFunction(fn)) {
      size_t s = 0U;
      char const* fn_name = LLVMGetValueName2(fn, &s);
      if (has_function_defined(fn)) {
        (*this)(fn, fn_name);
      }
    }
  }

private:
  LLVMModuleRef m_mod;
  decltype(make_fpm(nullptr)) m_fpm;
};

struct mod_pass_manager_t {
  mod_pass_manager_t() {
    LLVMAddFunctionInliningPass(m_mpm.get());
    LLVMAddMergeFunctionsPass(m_mpm.get());
  }

  void operator()(LLVMModuleRef mod) {
    size_t s{};
    char const* name = LLVMGetModuleIdentifier(mod, &s);
    std::cerr << "---------- module `" << name << "` before optimization\n";
    LLVMDumpModule(mod);
    if (LLVMRunPassManager(m_mpm.get(), mod) != 0) {
      std::cerr << "---------- module `" << name << "` after optimization " << '\n';
      LLVMDumpModule(mod);
    }
    std::cerr << "\n\n";
  }

private:
  decltype(make_mpm()) m_mpm{make_mpm()};
};

void verify_module(LLVMModuleRef mod) {
  char* error = nullptr;
  LLVMVerifyModule(mod, LLVMAbortProcessAction, &error);
  LLVMDisposeMessage(error);
}

LLVMErrorRef optimize_module(void* /*payload*/, LLVMModuleRef mod) {
  verify_module(mod);

  fn_pass_manager_t fpm{mod};
  fpm();

  mod_pass_manager_t mpm{};
  mpm(mod);

  return nullptr;
}

LLVMErrorRef optimize_tsm(void* /*payload*/, LLVMOrcThreadSafeModuleRef* tsm,
                          LLVMOrcMaterializationResponsibilityRef /*unused*/) {
  return LLVMOrcThreadSafeModuleWithModuleDo(*tsm, optimize_module, nullptr);
}

struct orc_v2_jit_t {
  orc_v2_jit_t() {
    LLVMInitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();

    LLVMErrorRef error = LLVMOrcCreateLLJIT(&m_jit, nullptr);
    if (m_jit == nullptr) {
      std::cerr << "create jit fail, " << LLVMGetErrorMessage(error) << '\n';
      assert(false); // NOLINT
    }
#ifdef HAS_BC062E0
    LLVMOrcIRTransformLayerRef ir_trans_layer = LLVMOrcLLJITGetIRTransformLayer(m_jit);
    LLVMOrcIRTransformLayerSetTransform(ir_trans_layer, optimize_tsm, nullptr); // leak?
#endif

    add_libc("puts", reinterpret_cast<uint64_t>(puts));           // NOLINT
    add_libc("say_hello", reinterpret_cast<uint64_t>(say_hello)); // NOLINT
  }
  orc_v2_jit_t(orc_v2_jit_t const&) = delete;
  orc_v2_jit_t(orc_v2_jit_t&&) = delete;
  orc_v2_jit_t& operator=(orc_v2_jit_t const&) = delete;
  orc_v2_jit_t& operator=(orc_v2_jit_t&&) = delete;
  ~orc_v2_jit_t() { LLVMOrcDisposeLLJIT(m_jit); }

  void add_module(LLVMOrcThreadSafeModuleRef tsm) {
#ifndef HAS_BC062E0
    LLVMConsumeError(optimize_tsm(nullptr, &tsm, nullptr));
#endif
    LLVMErrorRef error = LLVMOrcLLJITAddLLVMIRModule(m_jit, LLVMOrcLLJITGetMainJITDylib(m_jit), tsm);
    if (error != nullptr) {
      LLVMOrcDisposeThreadSafeModule(tsm);
      std::cerr << "add module fail, " << LLVMGetErrorMessage(error) << '\n';
      assert(false); // NOLINT
    }
  }

  LLVMOrcExecutorAddress lookup(char const* name) {
    LLVMOrcExecutorAddress addr{};
    LLVMErrorRef error = LLVMOrcLLJITLookup(m_jit, &addr, name);
    if (error != nullptr) {
      std::cerr << "lookup fail on `" << name << "`, " << LLVMGetErrorMessage(error) << '\n';
      assert(false); // NOLINT
    }
    return addr;
  }

private:
  void add_libc(char const* name, LLVMOrcJITTargetAddress addr) {
    LLVMJITSymbolFlags flags = {LLVMJITSymbolGenericFlagsWeak, 0};
    LLVMJITEvaluatedSymbol sym = {addr, flags}; // NOLINT

    LLVMOrcSymbolStringPoolEntryRef manged_name = LLVMOrcLLJITMangleAndIntern(m_jit, name);
    LLVMJITCSymbolMapPair pair = {manged_name, sym};
    auto pairs = std::array{pair};

    LLVMOrcMaterializationUnitRef mu = LLVMOrcAbsoluteSymbols(pairs.data(), 1);
    LLVMErrorRef error = LLVMOrcJITDylibDefine(LLVMOrcLLJITGetMainJITDylib(m_jit), mu);
    if (error != LLVMErrorSuccess) {
      LLVMOrcDisposeMaterializationUnit(mu);
      std::cerr << "create external define `" << name << "` failed, " << LLVMGetErrorMessage(error) << '\n';
      assert(false); // NOLINT
    }
  }

  LLVMOrcLLJITRef m_jit{};
};

struct tsm_t {
  using fn_t = LLVMValueRef (*)(LLVMModuleRef);
  void add(fn_t fn) const {
    struct w_t {
      fn_t f;
    };
    w_t w{.f = fn};
    LLVMOrcThreadSafeModuleWithModuleDo(
        m_tsm,
        [](void* ctx, LLVMModuleRef mod) -> LLVMErrorRef {
          reinterpret_cast<w_t*>(ctx)->f(mod); // NOLINT
          return nullptr;
        },
        static_cast<void*>(&w));
  }

  LLVMOrcThreadSafeModuleRef get() const { return m_tsm; }

private:
  static char const* get_new_mod_name() {
    static std::vector<std::string> m_names;
    static std::mutex m_names_mutex;
    static std::atomic<int> m_i{0};

    auto const i = ++m_i;
    std::string name = "mod-" + std::to_string(i);

    {
      const std::lock_guard lg{m_names_mutex};
      m_names.push_back(name);
      return m_names.back().c_str();
    }
  }

  static LLVMOrcThreadSafeModuleRef make_tsm(char const* name) {
    auto tsc = make_tsc();
    LLVMContextRef ctx = LLVMOrcThreadSafeContextGetContext(tsc.get());
    LLVMModuleRef mod = LLVMModuleCreateWithNameInContext(name, ctx);

    LLVMOrcThreadSafeModuleRef tsm = LLVMOrcCreateNewThreadSafeModule(mod, tsc.get());
    return tsm;
  }

  LLVMOrcThreadSafeModuleRef m_tsm{make_tsm(get_new_mod_name())};
};
} // namespace

namespace orc_v2_lib {
void all() {
  tsm_t std_tsm;
  std_tsm.add(create_out_fn);

  tsm_t tsm;
  tsm.add(create_factorial_fn);

  tsm_t tsm_cal_fact_4;
  tsm_cal_fact_4.add(create_cal_fact_4_fn);

  orc_v2_jit_t jit;
  jit.add_module(std_tsm.get());
  jit.add_module(tsm.get());
  jit.add_module(tsm_cal_fact_4.get());

  std::cerr << "----------------------------------------------------\n";

  auto cal_fact_4_addr = jit.lookup("cal_fact_4");
  exec_cal_fact_4_fn(cal_fact_4_addr);

  LLVMShutdown();
}
} // namespace orc_v2_lib

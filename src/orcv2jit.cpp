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
#include <cassert>
#include <cinttypes>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <memory>

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

bool has_function_defined(LLVMValueRef fn) { return LLVMCountBasicBlocks(fn) == 0; }

/**
 * int64 factorial(int64 n) {
 *   if (n == 0) return 1;
 *   return n * factorial(n-1);
 * }
 */
LLVMValueRef create_factorial_fn(LLVMModuleRef mod) {
  LLVMContextRef ctx = LLVMGetModuleContext(mod);

  LLVMTypeRef llvm_int_ty = LLVMInt64TypeInContext(ctx);

  LLVMTypeRef proto_ty = LLVMFunctionType(llvm_int_ty, &llvm_int_ty, 1U, /*isVarArg*/ 0);
  LLVMValueRef fn = LLVMAddFunction(mod, "factorial", proto_ty);

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

native_int_t exec_factorial_fn(uint64_t addr, native_int_t n) {       // NOLINT
  auto func = reinterpret_cast<native_int_t (*)(native_int_t)>(addr); // NOLINT
  return func(n);
}

struct extern_fn_t {
  LLVMTypeRef proto_ty;
  LLVMValueRef fn;
};

/**
 * int puts(const char* s);
 */
extern_fn_t declare_puts_fn(LLVMModuleRef mod) {
  LLVMContextRef ctx = LLVMGetModuleContext(mod);

  LLVMTypeRef char_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(ctx), /*AddressSpace*/ 0);
  LLVMTypeRef proto_ty = LLVMFunctionType(LLVMInt32TypeInContext(ctx), &char_ptr_ty, 1U, /*isVarArg*/ 0);
  LLVMValueRef puts_fn = LLVMAddFunction(mod, "puts", proto_ty);

  assert(has_function_defined(puts_fn)); // NOLINT
  return {.proto_ty = proto_ty, .fn = puts_fn};
}

/**
 * int out(const char* s)
 */
LLVMValueRef create_out_fn(LLVMModuleRef mod) {
  LLVMContextRef ctx = LLVMGetModuleContext(mod);

  auto [puts_proto_ty, puts_fn] = declare_puts_fn(mod);

  LLVMValueRef out_fn = LLVMAddFunction(mod, "out", puts_proto_ty);
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

auto exec_out_fn(uint64_t addr, char const* s) {
  auto func = reinterpret_cast<int (*)(char const*)>(addr); // NOLINT
  return func(s);
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
      (*this)(fn, fn_name);
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
    std::cerr << "---------- module before optimization\n";
    LLVMDumpModule(mod);
    if (LLVMRunPassManager(m_mpm.get(), mod) != 0) {
      std::cerr << "---------- module after optimization " << '\n';
      LLVMDumpModule(mod);
    }
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
  LLVMOrcLLJITRef m_jit{};
};
} // namespace

namespace orc_v2_lib {
void all() {
  auto tsc = make_tsc();
  LLVMOrcThreadSafeModuleRef tsm =
      LLVMOrcCreateNewThreadSafeModule(LLVMModuleCreateWithName("orc-v2-jit-mod"), tsc.get());

  // char const* out_name = "out";
  // auto* const out_fn = create_out_fn(mod);

  char const* fact_name = "factorial";
  LLVMOrcThreadSafeModuleWithModuleDo(
      tsm,
      [](void*, LLVMModuleRef mod) -> LLVMErrorRef {
        create_factorial_fn(mod);
        return nullptr;
      },
      nullptr);

  orc_v2_jit_t jit;
  jit.add_module(tsm);

  std::cerr << "----------------------------------------------------\n";

  // auto out_addr = jit.lookup(out_name);
  // exec_out_fn(out_addr, "hello, out");

  auto fact_addr = jit.lookup(fact_name);
  assert(exec_factorial_fn(fact_addr, 4) == 24LL);      // NOLINT
  std::cerr << exec_factorial_fn(fact_addr, 4) << '\n'; // NOLINT

  LLVMShutdown();
}
} // namespace orc_v2_lib

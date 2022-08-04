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
#include <llvm-c/ExecutionEngine.h>
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

auto make_builder() {
  return std::shared_ptr<std::remove_pointer_t<LLVMBuilderRef>>{LLVMCreateBuilder(), LLVMDisposeBuilder};
}

auto make_tsc() {
  return std::shared_ptr<std::remove_pointer_t<LLVMOrcThreadSafeContextRef>>{LLVMOrcCreateNewThreadSafeContext(),
                                                                             LLVMOrcDisposeThreadSafeContext};
}

bool has_function_defined(LLVMValueRef fn) { return LLVMCountBasicBlocks(fn) == 0; }

/**
 * int64 factorial(int64 n) {
 *   if (n == 0) return 1;
 *   return n * factorial(n-1);
 * }
 */
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

struct extern_fn_t {
  LLVMTypeRef proto_ty;
  LLVMValueRef fn;
};

/**
 * int puts(const char* s);
 */
extern_fn_t declare_puts_fn(LLVMModuleRef mod) {
  LLVMTypeRef char_ptr_ty = LLVMPointerType(LLVMInt8Type(), /*AddressSpace*/ 0);
  LLVMTypeRef proto_ty = LLVMFunctionType(LLVMInt32Type(), &char_ptr_ty, 1U, /*isVarArg*/ 0);
  LLVMValueRef puts_fn = LLVMAddFunction(mod, "puts", proto_ty);

  assert(has_function_defined(puts_fn)); // NOLINT
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
    LLVMAddLoopDeletionPass(m_fpm);
    LLVMAddLoopIdiomPass(m_fpm);
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

void optimize_module(LLVMModuleRef mod) {
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

void verify_module(LLVMModuleRef mod) {
  char* error = nullptr;
  LLVMVerifyModule(mod, LLVMAbortProcessAction, &error);
  LLVMDisposeMessage(error);
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
  }
  orc_v2_jit_t(orc_v2_jit_t const&) = delete;
  orc_v2_jit_t(orc_v2_jit_t&&) = delete;
  orc_v2_jit_t& operator=(orc_v2_jit_t const&) = delete;
  orc_v2_jit_t& operator=(orc_v2_jit_t&&) = delete;
  ~orc_v2_jit_t() { LLVMOrcDisposeLLJIT(m_jit); }

  void add_module(LLVMOrcThreadSafeModuleRef tsm) {
    LLVMOrcThreadSafeModuleWithModuleDo(
        tsm,
        [](void*, LLVMModuleRef mod) -> LLVMErrorRef {
          verify_module(mod);

          fn_pass_manager_t fpm{mod};

          for (LLVMValueRef fn = LLVMGetFirstFunction(mod); fn != nullptr; fn = LLVMGetNextFunction(fn)) {
            size_t s = 0U;
            const char* fn_name = LLVMGetValueName2(fn, &s);
            fpm(fn, fn_name);
          }
          optimize_module(mod);
          return nullptr;
        },
        nullptr);
    LLVMErrorRef error = LLVMOrcLLJITAddLLVMIRModule(m_jit, LLVMOrcLLJITGetMainJITDylib(m_jit), tsm);
    if (error != nullptr) {
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

  // fn_pass_manager_t fpm{mod};

  // char const* out_name = "out";
  // auto* const out_fn = create_out_fn(mod);
  // fpm(out_fn, out_name);

  char const* fact_name = "factorial";
  LLVMOrcThreadSafeModuleWithModuleDo(
      tsm,
      [](void*, LLVMModuleRef mod) -> LLVMErrorRef {
        create_factorial_fn(mod);
        return nullptr;
      },
      nullptr);
  // fpm(fact_fn, fact_name);

  orc_v2_jit_t jit;
  jit.add_module(tsm);

  std::cerr << "----------------------------------------------------\n";

  // auto out_addr = jit.lookup(out_name);
  // exec_out_fn(out_addr, "hello, out");

  auto fact_addr = jit.lookup(fact_name);
  assert(exec_factorial_fn(fact_addr, 4) == 24LL);      // NOLINT
  std::cerr << exec_factorial_fn(fact_addr, 4) << '\n'; // NOLINT
}
} // namespace orc_v2_lib

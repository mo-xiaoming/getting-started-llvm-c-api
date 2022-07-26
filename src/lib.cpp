#include "lib.hpp"

/**
 * LLVM equivalent of:
 *
 * int sum(int a, int b) {
 *     return a + b;
 * }
 */

#include <cassert>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/Transforms/IPO.h>
#include <llvm-c/Transforms/Scalar.h>
#include <llvm-c/Transforms/Utils.h>

#include <array>
#include <cinttypes>
#include <cstdio>
#include <cstdlib>
#include <iostream>

namespace {
LLVMValueRef create_sum_fn(LLVMModuleRef mod) {
  auto* const llvm_int_ty = LLVMInt64Type();

  auto param_types = std::array{llvm_int_ty, llvm_int_ty};
  LLVMTypeRef proto_type = LLVMFunctionType(llvm_int_ty, param_types.data(), 2U, /*isVarArg*/ 0);
  LLVMValueRef sum_fn = LLVMAddFunction(mod, "sum", proto_type);

  LLVMBasicBlockRef entry = LLVMAppendBasicBlock(sum_fn, "entry");

  LLVMBuilderRef builder = LLVMCreateBuilder();
  LLVMPositionBuilderAtEnd(builder, entry);
  LLVMValueRef add_tmp = LLVMBuildAdd(builder, LLVMGetParam(sum_fn, 0), LLVMGetParam(sum_fn, 1), "add_tmp");
  LLVMBuildRet(builder, add_tmp);
  LLVMDisposeBuilder(builder);

  LLVMVerifyFunction(sum_fn, LLVMAbortProcessAction);
  return sum_fn;
}

auto to_int(char const* s) { return std::strtoll(s, nullptr, 10); }

auto exec_sum_fn(uint64_t addr, char const* a, char const* b) {
  using lib::native_int_t;
  auto func = reinterpret_cast<native_int_t (*)(native_int_t, native_int_t)>(addr); // NOLINT
  native_int_t x = to_int(a);
  native_int_t y = to_int(b);
  return func(x, y);
}
} // namespace

namespace lib {
static constexpr auto Fn_Opt_Cnt = 1;
static constexpr auto Module_Opt_Cnt = 1;

struct fn_pass_manager_t {
  explicit fn_pass_manager_t(LLVMModuleRef mod) : m_fpm{LLVMCreateFunctionPassManagerForModule(mod)} {
    LLVMAddBasicAliasAnalysisPass(m_fpm);
    LLVMAddPromoteMemoryToRegisterPass(m_fpm);
    LLVMAddInstructionCombiningPass(m_fpm);
    LLVMAddInstructionSimplifyPass(m_fpm);
    LLVMAddReassociatePass(m_fpm);
    LLVMAddGVNPass(m_fpm);
    LLVMAddCFGSimplificationPass(m_fpm);
    LLVMInitializeFunctionPassManager(m_fpm);
  }
  fn_pass_manager_t(fn_pass_manager_t const&) = delete;
  fn_pass_manager_t(fn_pass_manager_t&&) = delete;
  fn_pass_manager_t& operator=(fn_pass_manager_t const&) = delete;
  fn_pass_manager_t& operator=(fn_pass_manager_t&&) = delete;
  ~fn_pass_manager_t() { LLVMDisposePassManager(m_fpm); }

  void operator()(LLVMValueRef fn, char const* name) {
    std::cerr << "-- function `" << name << "` before optimization\n";
    for (auto i = 0; i < Fn_Opt_Cnt; ++i) {
      if (LLVMRunFunctionPassManager(m_fpm, fn) == 0) {
        break;
      }
      std::cerr << "-- function `" << name << "` after iteration " << i << '\n';
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
  std::cerr << "-- module before optimization\n";
  LLVMDumpModule(mod);
  for (auto i = 0; i < Module_Opt_Cnt; ++i) {
    if (LLVMRunPassManager(mpm, mod) == 0) {
      break;
    }
    std::cerr << "-- after iteration " << i << '\n';
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

native_int_t sum(char const* a, char const* b) {
  LLVMModuleRef mod = LLVMModuleCreateWithName("jit-mod");

  fn_pass_manager_t fpm{mod};

  char const* sum_name = "sum";
  auto* const sum_fn = create_sum_fn(mod);
  fpm(sum_fn, sum_name);

  verify_module(mod);

  jit_t jit{mod};
  auto addr = jit.fn_addr(sum_name);
  return exec_sum_fn(addr, a, b);
}
} // namespace lib

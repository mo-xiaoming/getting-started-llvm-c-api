#[[
init_llvm()
target_add_llvm(TARGET, "core;orcjit;nativecodegen")
]]

macro(init_llvm)
  find_package(LLVM CONFIG REQUIRED)
  include(HandleLLVMOptions)
  message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION} with ASSERTIONS ${LLVM_ENABLE_ASSERTIONS}, EH ${LLVM_ENABLE_EH}, RTTI ${LLVM_ENABLE_RTTI}")
  message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
  message(STATUS "Using LLVM binary tools in: ${LLVM_TOOLS_BINARY_DIR}")
endmacro(init_llvm)

function(target_add_llvm target components)
  llvm_map_components_to_libnames(llvm_libs ${components})
  message(STATUS "${target} adds LLVM libs: ${llvm_libs}")
  target_link_libraries(${target} PRIVATE ${llvm_libs})

  message(STATUS "${target} adds LLVM includes: ${LLVM_INCLUDE_DIRS}")
  target_include_directories(${target} SYSTEM PRIVATE ${LLVM_INCLUDE_DIRS})

  message(STATUS "${target} adds LLVM definations: ${LLVM_DEFINITIONS}")
  target_compile_definitions(${target} PRIVATE ${LLVM_DEFINITIONS})
endfunction(target_add_llvm)


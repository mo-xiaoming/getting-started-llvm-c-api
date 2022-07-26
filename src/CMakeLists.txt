init_llvm()

add_library(getting-started-llvm-c-api-lib lib.cpp)
target_link_libraries(getting-started-llvm-c-api-lib PUBLIC project_defaults)
target_add_llvm(getting-started-llvm-c-api-lib "core;nativecodegen;mcjit;passes")

add_executable(getting-started-llvm-c-api main.cpp)
target_link_libraries(getting-started-llvm-c-api PRIVATE getting-started-llvm-c-api-lib)

install(TARGETS
  getting-started-llvm-c-api-lib
  getting-started-llvm-c-api
  DESTINATION ${CMAKE_BINARY_DIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
)

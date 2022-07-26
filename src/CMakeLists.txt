init_llvm()

add_library(llvm-c-api-lib lib.cpp)
target_link_libraries(llvm-c-api-lib PUBLIC project_defaults)
target_add_llvm(llvm-c-api-lib "core;nativecodegen;mcjit;passes")

add_executable(llvm-c-api-test test.cpp)
target_link_libraries(llvm-c-api-test PRIVATE llvm-c-api-lib GTest::gtest GTest::gtest_main)
gtest_discover_tests(llvm-c-api-test)

add_executable(llvm-c-api main.cpp)
target_link_libraries(llvm-c-api PRIVATE llvm-c-api-lib)

install(TARGETS
  llvm-c-api-lib
  llvm-c-api
  llvm-c-api-test
  DESTINATION ${CMAKE_BINARY_DIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
)

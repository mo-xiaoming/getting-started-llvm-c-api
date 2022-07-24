#init_llvm()

add_library(cpp-vcpkg-template-lib lib.cpp)
target_link_libraries(cpp-vcpkg-template-lib PUBLIC project_defaults)
#target_add_llvm(cpp-vcpkg-template-lib "core;nativecodegen;mcjit")

add_executable(cpp-vcpkg-template-test test.cpp)
target_link_libraries(cpp-vcpkg-template-test PRIVATE cpp-vcpkg-template-lib GTest::gtest GTest::gtest_main)
gtest_discover_tests(cpp-vcpkg-template-test)

add_executable(cpp-vcpkg-template main.cpp)
target_link_libraries(cpp-vcpkg-template PRIVATE cpp-vcpkg-template-lib)

install(TARGETS
  cpp-vcpkg-template-lib
  cpp-vcpkg-template
  cpp-vcpkg-template-test
  DESTINATION ${CMAKE_BINARY_DIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
)

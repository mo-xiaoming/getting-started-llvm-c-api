cmake_minimum_required(VERSION 3.12) # needs this for c++20
project(cpp-vcpkg-template VERSION 0.1.0 DESCRIPTION "cpp vcpkg template" HOMEPAGE_URL "https://github.com/mo-xiaoming/cpp-vcpkg-template")

include(GNUInstallDirs)

include(cmake/ProjectDefaults.cmake)
#include(cmake/LLVMProject.cmake)

list(APPEND CMAKE_CTEST_ARGUMENTS "--output-on-failure")
find_package(GTest REQUIRED)
enable_testing()
include(CTest)
include(GoogleTest)

add_subdirectory(src)

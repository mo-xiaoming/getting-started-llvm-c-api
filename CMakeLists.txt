cmake_minimum_required(VERSION 3.12) # needs this for c++20
project(getting-started-llvm-c-api VERSION 0.1.0 DESCRIPTION "getting started with llvm c api" HOMEPAGE_URL "https://github.com/mo-xiaoming/getting-started-llvm-c-api")

include(GNUInstallDirs)

include(cmake/ProjectDefaults.cmake)
include(cmake/LLVMProject.cmake)

add_subdirectory(src)

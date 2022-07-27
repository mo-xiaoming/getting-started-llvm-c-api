# Getting started LLVM C API

Started from Pauladam Smith's blog [how to get started with llvm c api](https://www.pauladamsmith.com/blog/2015/01/how-to-get-started-with-llvm-c-api.html), then try to make it a comprehensive code guide about llvm c api

1. Adopted Paladam's code to LLVM 14 and C++
2. Call functions from libc, `puts`
3. Recursive function `factorial(n)` with `LLVMAddTailCallEliminationPass` to elimate recursive calls in IR

## Build

Typical cmake project, make sure LLVM in cmake search path


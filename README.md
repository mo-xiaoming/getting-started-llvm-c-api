# Getting started LLVM C API

Started from Pauladam Smith's blog [how to get started with llvm c api](https://www.pauladamsmith.com/blog/2015/01/how-to-get-started-with-llvm-c-api.html), then try to make it a comprehensive code guide about llvm c api

1. Adopted Paladam's `sum` code to LLVM 14 and C++
2. Call functions from libc, `puts`
3. Recursive function `factorial(n)` with `LLVMAddTailCallEliminationPass` to elimate recursive calls in IR
4. Naive recursive implementation of `fib(n)`, clearly only the last `fib` call in `fib(n-1) + fib(n-2)` got TCE optimized
5. `if then else` pattern
6. `while-do` without PHI. For factorial, this version and the recursive one optimized down to the same IR code

## Build

Typical cmake project, make sure LLVM in cmake search path


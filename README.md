# Getting started LLVM C API

Started from Pauladam Smith's blog [how to get started with llvm c api](https://www.pauladamsmith.com/blog/2015/01/how-to-get-started-with-llvm-c-api.html), then try to make it a comprehensive code guide about llvm c api

1. Adopted Paladam's `sum` code to LLVM 14 and C++. **simple function** -- `create_sum_fn`
2. Call functions from libc, `puts`. **call function in libc** -- `create_out_fn`
3. Recursive function `factorial(n)` with `LLVMAddTailCallEliminationPass` to eliminate recursive calls in IR. **recursive calls and TCE** -- `create_factorial_fn`
4. Naive recursive implementation of `fib(n)`, clearly only the last `fib` call in `fib(n-1) + fib(n-2)` got TCE optimized. **more complicated recursive calls and TCE** -- `create_fib_fn`
5. `if then else` pattern. **if-the-else with PHI** -- `create_is_odd_fn`
6. `while-do` without PHI. For factorial, this version and the recursive one optimized down to the same IR code. **while-do without PHI** -- `create_factorial_loop_fn`
7. Sum of an array of arithmetic progression numbers, I really hoped LLVM can do it in the way of `(f + l) * n / 2`, but sadly no, still loops. **array access with GEP** -- `create_ap_sum_fn`

## Build

Typical cmake project, make sure LLVM in cmake search path


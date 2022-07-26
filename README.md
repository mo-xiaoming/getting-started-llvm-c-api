# Getting started LLVM C API

Started from Pauladam Smith's blog [how to get started with llvm c api](https://www.pauladamsmith.com/blog/2015/01/how-to-get-started-with-llvm-c-api.html), then try to make it a comprehensive code guide about llvm c api

## Build

Typical cmake project, make sure LLVM in cmake search path

## MCJIT

1. **simple function** -- `create_sum_fn`: Adopted Paladam's `sum` code to LLVM 14 and C++
2. **call function in libc** -- `create_out_fn`: Call functions from libc, `puts`
3. **recursive calls and TCE** -- `create_factorial_fn`: Recursive function `factorial(n)` with `LLVMAddTailCallEliminationPass` to eliminate recursive calls in IR
4. **more complicated recursive calls and TCE** -- `create_fib_fn`: Naive recursive implementation of `fib(n)`, clearly only the last `fib` call in `fib(n-1) + fib(n-2)` got TCE optimized
5. **if-the-else with PHI** -- `create_is_odd_fn`: `if then else` pattern
6. **while-do without PHI** -- `create_factorial_loop_fn`: `while-do` without PHI. For factorial, this version and the recursive one optimized down to the same IR code.
7. **array access with GEP** -- `create_ap_sum_fn`: Sum of an array of arithmetic progression numbers, I really hoped LLVM can do it in the way of `(f + l) * n / 2`, but sadly no, still loops
8. **Niubi LLVM optimization** -- `create_ap_simple_sum_fn`: Sum of ~an array of~ a sequence of arithmetic progression numbers, still not using that formula, maybe because of some optimization are missing, [unlike pure -O3](https://godbolt.org/z/Pcxs6P3Wz), maybe it is being done during re-write

## ORC V2 JIT

In `orcv2jit.cpp` file

1. Found memory leaks in [llvm official example](https://github.com/llvm/llvm-project/blob/main/llvm/examples/OrcV2Examples/OrcV2CBindingsIRTransforms/OrcV2CBindingsIRTransforms.c), filed a [bug report](https://github.com/llvm/llvm-project/issues/56953), fixed in [4246269](https://github.com/llvm/llvm-project/commit/424626953e89ec555501320ce06513aebf8e6aec)
2. In the same bug report, I mentioned `LLVMOrcIRTransformLayerSetTransform` causes [memory leaks](https://github.com/llvm/llvm-project/issues/56953), fixed in [bc062e0](https://github.com/llvm/llvm-project/commit/bc062e034fbdb5674fc26d354ef5b4c236aab640)
3. `cal_fact_4`: call to another function in a different module

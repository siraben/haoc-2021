# Haskell Advent of Code 2021
My solutions for Advent of Code 2021 in Haskell.  Here's some goals I
set to make the most out of it:

1. No looking up solutions/discussing with others before completion.
2. No use of libraries outside of the [GHC bootstrap
  libraries](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/index.html)
  for the solutions.
3. If time permits, use
  [Criterion](https://hackage.haskell.org/package/criterion) to
  benchmark and improve solutions.
4. No unsafe Haskell.

The reason for (2) is that many online competitive programming sites
that support Haskell (CodeForces, Google Code Jam) do not have
libraries beyond the bootstrap list.  Plus, there's already a wealth
of competitive libraries in there, such as `bytestring`, `text`,
`parsec`, `containers` (which contains maps, sets, int sets, graphs,
sequences) and more.

I might do a full writeup after my semester is over, but here's my
rough procedure on how to tackle the problems.

1. Write the most naive thing that could possibly work.  If it works,
   submit the answers!
2. Focus on algorithmic improvements.  I use a combination of
   techniques:
   - use equational reasoning to fuse folds, traversals
   - use more efficient data structures
3. Focus on empirical improvements.  I make heavy use of Criterion,
   though [AutoBench](https://github.com/mathandley/AutoBench) seems
   interesting.
   - manually inlining helper functions and equational reasoning, tail
     recursion
   - convert `foldr` to `foldl` when possible
   - using strict versions of functions, bang patterns
   - explicit type annotations
   - faster types: `Int` instead of `Integer`, `ByteString` instead of
     `String`, `Sequence` or `Vector` instead of `List`

Of course with (3) one could continue shaving off more and more time,
though these heuristics in practice have given me most of the gains.

## Writing Haskell for AoC quickly
AoC is all about solving the problem quickly via any means, so

> a naive (but possibly inefficient) solution that is quick to write
> and produces the answer is better than an optimized one that is slow
> to write

From this principle, it informs my choices on how to write Haskell
quickly for the initial solve.

- put everything in main, variables/expressions used more than once go
  in a `let` expression, use a short, random identifier unless you can
  think of a good one within 3 seconds
- for mapping over lists, use `<$>`, but if you need to filter, use
  `let` expressions or iterate over a cartesian product of lists, list
  comprehensions/monadic syntax is better
- never discard parts of input even if it makes part 1 faster to
  solve, since almost always part 2 will use that information and
  you'll have to adjust the `String -> data` step again
- avoid explicit recursion when possible, or at the least use tail
  recursion with a function named `go`
- print intermediate results as you process data, especially if the
  transformation is complex, so `do { let a = f x; print a; let b = g
  a; ... }` over `do { print (f (g x)) }`
- if using the state monad (should be a last resort), use a record for
  the state

## Best benchmarks so far
<details>
<summary>CPU details</summary>

```
Architecture:                    x86_64
CPU op-mode(s):                  32-bit, 64-bit
Byte Order:                      Little Endian
Address sizes:                   39 bits physical, 48 bits virtual
CPU(s):                          4
On-line CPU(s) list:             0-3
Thread(s) per core:              2
Core(s) per socket:              2
Socket(s):                       1
NUMA node(s):                    1
Vendor ID:                       GenuineIntel
CPU family:                      6
Model:                           69
Model name:                      Intel(R) Core(TM) i5-4288U CPU @ 2.60GHz
```
</details>

### Day 1
<details>

```
benchmarking day1/part1
time                 13.00 μs   (12.91 μs .. 13.11 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 12.95 μs   (12.88 μs .. 13.05 μs)
std dev              296.4 ns   (193.5 ns .. 512.7 ns)
variance introduced by outliers: 23% (moderately inflated)

benchmarking day1/part2
time                 62.31 μs   (57.58 μs .. 67.19 μs)
                     0.962 R²   (0.947 R² .. 0.984 R²)
mean                 55.53 μs   (53.36 μs .. 59.03 μs)
std dev              8.827 μs   (5.825 μs .. 12.66 μs)
variance introduced by outliers: 93% (severely inflated)
```
</details>

### Day 2
<details>

```
benchmarking day2/part1
time                 6.886 μs   (6.751 μs .. 7.057 μs)
                     0.982 R²   (0.957 R² .. 0.996 R²)
mean                 7.487 μs   (7.101 μs .. 8.268 μs)
std dev              1.779 μs   (1.044 μs .. 3.087 μs)
variance introduced by outliers: 98% (severely inflated)

benchmarking day2/part2
time                 12.69 μs   (12.41 μs .. 12.99 μs)
                     0.997 R²   (0.995 R² .. 0.998 R²)
mean                 12.50 μs   (12.33 μs .. 12.72 μs)
std dev              638.9 ns   (526.8 ns .. 789.7 ns)
variance introduced by outliers: 61% (severely inflated)
```
</details>

### Day 3
<details>

```
benchmarking day3/part1
time                 322.3 μs   (315.1 μs .. 331.9 μs)
                     0.996 R²   (0.993 R² .. 0.999 R²)
mean                 321.4 μs   (318.4 μs .. 326.6 μs)
std dev              12.40 μs   (7.332 μs .. 18.52 μs)
variance introduced by outliers: 34% (moderately inflated)

benchmarking day3/part2
time                 292.1 μs   (290.1 μs .. 293.9 μs)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 296.2 μs   (294.3 μs .. 301.1 μs)
std dev              9.972 μs   (4.409 μs .. 17.62 μs)
variance introduced by outliers: 28% (moderately inflated)
```
</details>


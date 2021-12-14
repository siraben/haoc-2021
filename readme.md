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

### Day 4
<details>

```
benchmarking day4/part1
time                 5.563 ms   (5.403 ms .. 5.712 ms)
                     0.994 R²   (0.990 R² .. 0.998 R²)
mean                 5.634 ms   (5.563 ms .. 5.745 ms)
std dev              268.0 μs   (195.6 μs .. 366.4 μs)
variance introduced by outliers: 24% (moderately inflated)

benchmarking day4/part2
time                 5.643 ms   (5.542 ms .. 5.799 ms)
                     0.995 R²   (0.991 R² .. 0.998 R²)
mean                 5.719 ms   (5.648 ms .. 5.872 ms)
std dev              280.5 μs   (184.5 μs .. 435.8 μs)
variance introduced by outliers: 27% (moderately inflated)
```
</details>

### Day 5
<details>

```
benchmarking day5/part1
time                 37.60 ms   (35.03 ms .. 40.00 ms)
                     0.980 R²   (0.955 R² .. 0.995 R²)
mean                 39.42 ms   (37.81 ms .. 41.46 ms)
std dev              3.688 ms   (2.252 ms .. 4.773 ms)
variance introduced by outliers: 38% (moderately inflated)

benchmarking day5/part2
time                 109.9 ms   (100.6 ms .. 121.7 ms)
                     0.988 R²   (0.971 R² .. 1.000 R²)
mean                 104.9 ms   (102.0 ms .. 110.0 ms)
std dev              6.127 ms   (1.795 ms .. 8.095 ms)
variance introduced by outliers: 10% (moderately inflated)
```
</details>

### Day 6
<details>

```
benchmarking day6/part1
time                 1.258 μs   (1.240 μs .. 1.294 μs)
                     0.990 R²   (0.972 R² .. 0.999 R²)
mean                 1.295 μs   (1.253 μs .. 1.381 μs)
std dev              180.6 ns   (83.29 ns .. 334.4 ns)
variance introduced by outliers: 94% (severely inflated)

benchmarking day6/part2
time                 4.469 μs   (4.423 μs .. 4.520 μs)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 4.460 μs   (4.435 μs .. 4.527 μs)
std dev              122.9 ns   (48.87 ns .. 242.0 ns)
variance introduced by outliers: 33% (moderately inflated)
```
</details>

### Day 7
<details>

```
benchmarking day7/part1
time                 8.115 ms   (8.043 ms .. 8.204 ms)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 8.208 ms   (8.135 ms .. 8.303 ms)
std dev              239.4 μs   (149.2 μs .. 308.6 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking day7/part2
time                 8.149 ms   (8.104 ms .. 8.211 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 8.219 ms   (8.184 ms .. 8.281 ms)
std dev              130.3 μs   (91.41 μs .. 189.0 μs)

```
</details>

### Day 8
<details>

```
benchmarking day8/part1
time                 56.09 μs   (55.44 μs .. 56.97 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 56.03 μs   (55.59 μs .. 56.67 μs)
std dev              1.833 μs   (1.280 μs .. 2.455 μs)
variance introduced by outliers: 34% (moderately inflated)

benchmarking day8/part2
time                 3.628 s    (3.466 s .. 3.915 s)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 3.490 s    (3.449 s .. 3.562 s)
std dev              67.94 ms   (7.585 ms .. 86.34 ms)
variance introduced by outliers: 19% (moderately inflated)
```
</details>

### Day 9
<details>

```
benchmarking day9/part1
time                 2.353 ms   (2.236 ms .. 2.461 ms)
                     0.967 R²   (0.936 R² .. 0.988 R²)
mean                 2.356 ms   (2.284 ms .. 2.460 ms)
std dev              287.6 μs   (213.8 μs .. 408.3 μs)
variance introduced by outliers: 77% (severely inflated)

benchmarking day9/part2
time                 5.345 ms   (5.013 ms .. 5.670 ms)
                     0.970 R²   (0.948 R² .. 0.986 R²)
mean                 6.062 ms   (5.647 ms .. 6.792 ms)
std dev              1.580 ms   (940.2 μs .. 2.361 ms)
variance introduced by outliers: 92% (severely inflated)
```
</details>

### Day 10
<details>

```
benchmarking day10/part1
time                 91.75 μs   (91.04 μs .. 92.53 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 91.59 μs   (91.32 μs .. 92.38 μs)
std dev              1.354 μs   (711.3 ns .. 2.608 μs)

benchmarking day10/part2
time                 8.692 μs   (8.597 μs .. 8.794 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 8.642 μs   (8.598 μs .. 8.715 μs)
std dev              182.8 ns   (139.4 ns .. 243.6 ns)
variance introduced by outliers: 22% (moderately inflated)
```
</details>

### Day 11
<details>

```
benchmarking day11/part1
time                 3.539 ms   (3.374 ms .. 3.750 ms)
                     0.984 R²   (0.972 R² .. 0.996 R²)
mean                 3.470 ms   (3.415 ms .. 3.544 ms)
std dev              210.7 μs   (153.6 μs .. 321.4 μs)
variance introduced by outliers: 39% (moderately inflated)

benchmarking day11/part2
time                 6.247 ms   (6.211 ms .. 6.279 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 6.322 ms   (6.297 ms .. 6.359 ms)
std dev              87.78 μs   (60.30 μs .. 142.7 μs)
```
</details>

### Day 13
<details>

```
benchmarking day13/part1
time                 177.4 μs   (175.8 μs .. 179.9 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 176.9 μs   (175.9 μs .. 178.5 μs)
std dev              4.002 μs   (2.264 μs .. 6.517 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking day13/part2
time                 35.00 ns   (34.65 ns .. 35.38 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 35.05 ns   (34.71 ns .. 35.97 ns)
std dev              1.796 ns   (716.6 ps .. 3.466 ns)
variance introduced by outliers: 73% (severely inflated)
```
</details>

### Day 14
<details>

```
benchmarking day14/part1
time                 656.1 μs   (652.6 μs .. 661.1 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 658.9 μs   (655.6 μs .. 671.3 μs)
std dev              18.75 μs   (8.364 μs .. 38.72 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking day14/part2
time                 3.931 ms   (3.869 ms .. 3.998 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 3.946 ms   (3.905 ms .. 4.110 ms)
std dev              228.3 μs   (76.05 μs .. 459.3 μs)
variance introduced by outliers: 36% (moderately inflated)
```
</details>

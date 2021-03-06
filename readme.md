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
time                 13.00 ??s   (12.91 ??s .. 13.11 ??s)
                     0.999 R??   (0.999 R?? .. 1.000 R??)
mean                 12.95 ??s   (12.88 ??s .. 13.05 ??s)
std dev              296.4 ns   (193.5 ns .. 512.7 ns)
variance introduced by outliers: 23% (moderately inflated)

benchmarking day1/part2
time                 62.31 ??s   (57.58 ??s .. 67.19 ??s)
                     0.962 R??   (0.947 R?? .. 0.984 R??)
mean                 55.53 ??s   (53.36 ??s .. 59.03 ??s)
std dev              8.827 ??s   (5.825 ??s .. 12.66 ??s)
variance introduced by outliers: 93% (severely inflated)
```
</details>

### Day 2
<details>

```
benchmarking day2/part1
time                 6.886 ??s   (6.751 ??s .. 7.057 ??s)
                     0.982 R??   (0.957 R?? .. 0.996 R??)
mean                 7.487 ??s   (7.101 ??s .. 8.268 ??s)
std dev              1.779 ??s   (1.044 ??s .. 3.087 ??s)
variance introduced by outliers: 98% (severely inflated)

benchmarking day2/part2
time                 12.69 ??s   (12.41 ??s .. 12.99 ??s)
                     0.997 R??   (0.995 R?? .. 0.998 R??)
mean                 12.50 ??s   (12.33 ??s .. 12.72 ??s)
std dev              638.9 ns   (526.8 ns .. 789.7 ns)
variance introduced by outliers: 61% (severely inflated)
```
</details>

### Day 3
<details>

```
benchmarking day3/part1
time                 322.3 ??s   (315.1 ??s .. 331.9 ??s)
                     0.996 R??   (0.993 R?? .. 0.999 R??)
mean                 321.4 ??s   (318.4 ??s .. 326.6 ??s)
std dev              12.40 ??s   (7.332 ??s .. 18.52 ??s)
variance introduced by outliers: 34% (moderately inflated)

benchmarking day3/part2
time                 292.1 ??s   (290.1 ??s .. 293.9 ??s)
                     0.999 R??   (0.996 R?? .. 1.000 R??)
mean                 296.2 ??s   (294.3 ??s .. 301.1 ??s)
std dev              9.972 ??s   (4.409 ??s .. 17.62 ??s)
variance introduced by outliers: 28% (moderately inflated)
```
</details>

### Day 4
<details>

```
benchmarking day4/part1
time                 5.563 ms   (5.403 ms .. 5.712 ms)
                     0.994 R??   (0.990 R?? .. 0.998 R??)
mean                 5.634 ms   (5.563 ms .. 5.745 ms)
std dev              268.0 ??s   (195.6 ??s .. 366.4 ??s)
variance introduced by outliers: 24% (moderately inflated)

benchmarking day4/part2
time                 5.643 ms   (5.542 ms .. 5.799 ms)
                     0.995 R??   (0.991 R?? .. 0.998 R??)
mean                 5.719 ms   (5.648 ms .. 5.872 ms)
std dev              280.5 ??s   (184.5 ??s .. 435.8 ??s)
variance introduced by outliers: 27% (moderately inflated)
```
</details>

### Day 5
<details>

```
benchmarking day5/part1
time                 37.60 ms   (35.03 ms .. 40.00 ms)
                     0.980 R??   (0.955 R?? .. 0.995 R??)
mean                 39.42 ms   (37.81 ms .. 41.46 ms)
std dev              3.688 ms   (2.252 ms .. 4.773 ms)
variance introduced by outliers: 38% (moderately inflated)

benchmarking day5/part2
time                 109.9 ms   (100.6 ms .. 121.7 ms)
                     0.988 R??   (0.971 R?? .. 1.000 R??)
mean                 104.9 ms   (102.0 ms .. 110.0 ms)
std dev              6.127 ms   (1.795 ms .. 8.095 ms)
variance introduced by outliers: 10% (moderately inflated)
```
</details>

### Day 6
<details>

```
benchmarking day6/part1
time                 1.258 ??s   (1.240 ??s .. 1.294 ??s)
                     0.990 R??   (0.972 R?? .. 0.999 R??)
mean                 1.295 ??s   (1.253 ??s .. 1.381 ??s)
std dev              180.6 ns   (83.29 ns .. 334.4 ns)
variance introduced by outliers: 94% (severely inflated)

benchmarking day6/part2
time                 4.469 ??s   (4.423 ??s .. 4.520 ??s)
                     0.999 R??   (0.997 R?? .. 1.000 R??)
mean                 4.460 ??s   (4.435 ??s .. 4.527 ??s)
std dev              122.9 ns   (48.87 ns .. 242.0 ns)
variance introduced by outliers: 33% (moderately inflated)
```
</details>

### Day 7
<details>

```
benchmarking day7/part1
time                 8.115 ms   (8.043 ms .. 8.204 ms)
                     0.998 R??   (0.995 R?? .. 1.000 R??)
mean                 8.208 ms   (8.135 ms .. 8.303 ms)
std dev              239.4 ??s   (149.2 ??s .. 308.6 ??s)
variance introduced by outliers: 11% (moderately inflated)

benchmarking day7/part2
time                 8.149 ms   (8.104 ms .. 8.211 ms)
                     0.999 R??   (0.998 R?? .. 1.000 R??)
mean                 8.219 ms   (8.184 ms .. 8.281 ms)
std dev              130.3 ??s   (91.41 ??s .. 189.0 ??s)

```
</details>

### Day 8
<details>

```
benchmarking day8/part1
time                 56.09 ??s   (55.44 ??s .. 56.97 ??s)
                     0.998 R??   (0.997 R?? .. 0.999 R??)
mean                 56.03 ??s   (55.59 ??s .. 56.67 ??s)
std dev              1.833 ??s   (1.280 ??s .. 2.455 ??s)
variance introduced by outliers: 34% (moderately inflated)

benchmarking day8/part2
time                 3.628 s    (3.466 s .. 3.915 s)
                     0.999 R??   (0.999 R?? .. 1.000 R??)
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
                     0.967 R??   (0.936 R?? .. 0.988 R??)
mean                 2.356 ms   (2.284 ms .. 2.460 ms)
std dev              287.6 ??s   (213.8 ??s .. 408.3 ??s)
variance introduced by outliers: 77% (severely inflated)

benchmarking day9/part2
time                 5.345 ms   (5.013 ms .. 5.670 ms)
                     0.970 R??   (0.948 R?? .. 0.986 R??)
mean                 6.062 ms   (5.647 ms .. 6.792 ms)
std dev              1.580 ms   (940.2 ??s .. 2.361 ms)
variance introduced by outliers: 92% (severely inflated)
```
</details>

### Day 10
<details>

```
benchmarking day10/part1
time                 91.75 ??s   (91.04 ??s .. 92.53 ??s)
                     1.000 R??   (1.000 R?? .. 1.000 R??)
mean                 91.59 ??s   (91.32 ??s .. 92.38 ??s)
std dev              1.354 ??s   (711.3 ns .. 2.608 ??s)

benchmarking day10/part2
time                 8.692 ??s   (8.597 ??s .. 8.794 ??s)
                     0.999 R??   (0.999 R?? .. 1.000 R??)
mean                 8.642 ??s   (8.598 ??s .. 8.715 ??s)
std dev              182.8 ns   (139.4 ns .. 243.6 ns)
variance introduced by outliers: 22% (moderately inflated)
```
</details>

### Day 11
<details>

```
benchmarking day11/part1
time                 3.539 ms   (3.374 ms .. 3.750 ms)
                     0.984 R??   (0.972 R?? .. 0.996 R??)
mean                 3.470 ms   (3.415 ms .. 3.544 ms)
std dev              210.7 ??s   (153.6 ??s .. 321.4 ??s)
variance introduced by outliers: 39% (moderately inflated)

benchmarking day11/part2
time                 6.247 ms   (6.211 ms .. 6.279 ms)
                     1.000 R??   (0.999 R?? .. 1.000 R??)
mean                 6.322 ms   (6.297 ms .. 6.359 ms)
std dev              87.78 ??s   (60.30 ??s .. 142.7 ??s)
```
</details>

### Day 12
<details>

```
benchmarking day12/part1
time                 7.916 ms   (7.420 ms .. 8.613 ms)
                     0.954 R??   (0.918 R?? .. 0.983 R??)
mean                 7.551 ms   (7.277 ms .. 7.912 ms)
std dev              913.1 ??s   (710.1 ??s .. 1.306 ms)
variance introduced by outliers: 66% (severely inflated)

benchmarking day12/part2
time                 330.5 ms   (283.5 ms .. 376.9 ms)
                     0.997 R??   (0.990 R?? .. 1.000 R??)
mean                 326.8 ms   (318.8 ms .. 333.5 ms)
std dev              8.079 ms   (5.410 ms .. 9.836 ms)
variance introduced by outliers: 19% (moderately inflated)
```
</details>

### Day 13
<details>

```
benchmarking day13/part1
time                 177.4 ??s   (175.8 ??s .. 179.9 ??s)
                     0.999 R??   (0.999 R?? .. 1.000 R??)
mean                 176.9 ??s   (175.9 ??s .. 178.5 ??s)
std dev              4.002 ??s   (2.264 ??s .. 6.517 ??s)
variance introduced by outliers: 16% (moderately inflated)

benchmarking day13/part2
time                 35.00 ns   (34.65 ns .. 35.38 ns)
                     0.998 R??   (0.997 R?? .. 0.999 R??)
mean                 35.05 ns   (34.71 ns .. 35.97 ns)
std dev              1.796 ns   (716.6 ps .. 3.466 ns)
variance introduced by outliers: 73% (severely inflated)
```
</details>

### Day 14
<details>

```
benchmarking day14/part1
time                 162.9 ??s   (161.8 ??s .. 164.2 ??s)
                     0.999 R??   (0.999 R?? .. 1.000 R??)
mean                 163.1 ??s   (162.4 ??s .. 164.4 ??s)
std dev              3.489 ??s   (2.391 ??s .. 5.648 ??s)
variance introduced by outliers: 15% (moderately inflated)

benchmarking day14/part2
time                 1.076 ms   (1.065 ms .. 1.087 ms)
                     0.999 R??   (0.998 R?? .. 0.999 R??)
mean                 1.067 ms   (1.062 ms .. 1.074 ms)
std dev              20.75 ??s   (16.45 ??s .. 26.09 ??s)
```
</details>

### Day 15
<details>

```
benchmarking day15/part1
time                 23.50 ms   (22.58 ms .. 24.61 ms)
                     0.995 R??   (0.990 R?? .. 0.999 R??)
mean                 23.91 ms   (23.36 ms .. 25.11 ms)
std dev              1.824 ms   (933.1 ??s .. 3.066 ms)
variance introduced by outliers: 33% (moderately inflated)

benchmarking day15/part2
time                 1.059 s    (1.025 s .. 1.100 s)
                     1.000 R??   (0.999 R?? .. 1.000 R??)
mean                 1.035 s    (1.022 s .. 1.047 s)
std dev              14.31 ms   (12.01 ms .. 15.56 ms)
variance introduced by outliers: 19% (moderately inflated)
```
</details>

### Day 16
<details>

```
benchmarking day16/part1
time                 1.732 ??s   (1.706 ??s .. 1.776 ??s)
                     0.960 R??   (0.881 R?? .. 0.998 R??)
mean                 1.942 ??s   (1.807 ??s .. 2.517 ??s)
std dev              796.2 ns   (201.9 ns .. 1.774 ??s)
variance introduced by outliers: 99% (severely inflated)

benchmarking day16/part2
time                 6.355 ??s   (6.287 ??s .. 6.452 ??s)
                     0.997 R??   (0.995 R?? .. 0.999 R??)
mean                 6.411 ??s   (6.326 ??s .. 6.586 ??s)
std dev              384.1 ns   (243.9 ns .. 648.0 ns)
variance introduced by outliers: 70% (severely inflated)
```
</details>

### Day 17
<details>

```
benchmarking day17/part1
time                 5.458 ns   (5.427 ns .. 5.495 ns)
                     1.000 R??   (0.999 R?? .. 1.000 R??)
mean                 5.469 ns   (5.442 ns .. 5.529 ns)
std dev              129.7 ps   (75.06 ps .. 241.3 ps)
variance introduced by outliers: 39% (moderately inflated)

benchmarking day17/part2
time                 21.29 ms   (20.95 ms .. 21.78 ms)
                     0.998 R??   (0.994 R?? .. 1.000 R??)
mean                 21.24 ms   (21.06 ms .. 21.58 ms)
std dev              574.4 ??s   (281.6 ??s .. 844.7 ??s)
```
</details>

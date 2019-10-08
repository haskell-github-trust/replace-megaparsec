# Revision history for replace-megaparsec

## 1.0.0.0 -- 2019-08-24

* First version.

## 1.0.1.0 -- 2019-08-28

* Add test suite.
* `sepCap` will treats `sep` as failing if it succeeds but consumes no input.

## 1.1.0.0 -- 2019-09-01

* Add benchmark suite.
* In `streamEditT`, replace `fold` with `mconcat`. The benchmarks now show
  linear scaling instead of quadratic.

## 1.1.5.0 -- 2019-10-08

* Move benchmarks to [__replace-benchmark__](https://github.com/jamesdbrock/replace-benchmark)

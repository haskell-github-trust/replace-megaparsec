# Revision history for replace-megaparsec

## 1.2.0.0 -- 2019-10-31

Benchmark improvements

Specializations of the `sepCap` function, guided by
[replace-benchmark](https://github.com/jamesdbrock/replace-benchmark).

### New benchmarks

| Program                                           | dense     | sparse   |
| :---                                              |      ---: |     ---: |
| `Replace.Megaparsec.streamEdit` `String`          | 454.95ms  | 375.04ms |
| `Replace.Megaparsec.streamEdit` `ByteString`      | 529.99ms  | 73.76ms  |
| `Replace.Megaparsec.streamEdit` `Text`            | 547.47ms  | 139.21ms |

### Old benchmarks

| Program                                           | dense     | sparse   |
| :---                                              |      ---: |     ---: |
|  `Replace.Megaparsec.streamEdit`     `String`     | 454.95ms  | 375.04ms |
|  `Replace.Megaparsec.streamEdit`     `ByteString` | 611.98ms  | 433.26ms |
|  `Replace.Megaparsec.streamEdit`     `Text`       | 592.66ms  | 353.32ms |

## 1.1.5.0 -- 2019-10-08

* Move benchmarks to [__replace-benchmark__](https://github.com/jamesdbrock/replace-benchmark)

## 1.1.0.0 -- 2019-09-01

* Add benchmark suite.
* In `streamEditT`, replace `fold` with `mconcat`. The benchmarks now show
  linear scaling instead of quadratic.

## 1.0.1.0 -- 2019-08-28

* Add test suite.
* `sepCap` will treats `sep` as failing if it succeeds but consumes no input.

## 1.0.0.0 -- 2019-08-24

* First version.


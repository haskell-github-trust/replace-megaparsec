# Revision history for replace-megaparsec

## 1.5.0.0 -- 2023-05-30

Upgrade to GHC v9.4.4, text v2.0.1

Text does not work with GHC v9.4.3

Test
* exitcode-stdio-1.0 instead of detailed-0.9
* HSpec instead of Cabal Distribution.TestSuite

Added megaparsec version bounds #36

## 1.4.5.0 -- 2022-04-14

Minor documentation changes.

Confirmed tests pass for text v2.

## 1.4.4.0 -- 2020-12-04

Add `splitCapT` and `breakCapT`.

## 1.4.3.0 -- 2020-09-28

Bugfix sepCap backtracking when sep fails

See [#33](https://github.com/jamesdbrock/replace-megaparsec/issues/33)

## 1.4.1.0 -- 2020-05-07

anyTill use getInput instead of takeRest

## 1.4.0.0 -- 2020-05-06

__Running Parsers__: Add `splitCap` and `breakCap`.

__Parser Combinators__: Add `anyTill`.

Remove `Show` and `Typeable` constraints on `streamEditT`.

## 1.3.0.0 -- 2020-03-06

`sepCap` won't throw.

Don't throw an exception on an unreachable error case, just bottom.
Remove type constraints for `Exception`.

## 1.2.1.0 -- 2020-01-01

Allow any error parameter, not just `Void`.

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


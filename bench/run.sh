#! /usr/bin/env bash

# Make a 1MB test file.
if [ ! -f bench-test.txt ]; then
    for ((i=1;i<=100000;i++))
    do
        echo "       foo" >> bench-test.txt
    done
fi

cabal v2-build bench-string
cabal v2-build bench-bytestring
cabal v2-build bench-text

# diff <(sed 's/foo/bar/g' < bench-test.txt) <(cabal v2-run bench-string < bench-test.txt)
# diff <(sed 's/foo/bar/g' < bench-test.txt) <(cabal v2-run bench-bytestring < bench-test.txt)

perf stat sed 's/foo/bar/g' < bench-test.txt 1> /dev/null
perf stat cabal v2-run bench-string < bench-test.txt 1> /dev/null
perf stat cabal v2-run bench-bytestring < bench-test.txt 1> /dev/null
perf stat cabal v2-run bench-text < bench-test.txt 1> /dev/null


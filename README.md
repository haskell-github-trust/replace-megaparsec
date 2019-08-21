# Parse Replace

This module is for doing “pattern capture” or
“stream editing” or “find-and-replace” or “match-and-substitute,” using
[__Megaparsec__](http://hackage.haskell.org/package/megaparsec)
parsers instead of the more traditional regular expressions.

It can be used
in the same sort of “pattern capture” situations in which
one would
use the Python
[`re.findall`](https://docs.python.org/3/library/re.html#re.findall)
or
Unix [`grep`](https://www.gnu.org/software/grep/).

This module can be used for “find-and-replace” or “stream editing” in the
same sort of situations in which
one would use Python
[`re.sub`](https://docs.python.org/3/library/re.html#re.sub)
, or Unix
[`sed` substitute](https://www.gnu.org/software/sed/manual/html_node/The-_0022s_0022-Command.html),
or
[`awk`](https://www.gnu.org/software/gawk/manual/gawk.html).


## Examples

Try the examples with `ghci` by
running `cabal v2-repl` in the `parse-replace/`
root directory.

The examples depend on these imports and definitions.

```haskell
import Parsereplace
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
let hexparser = string "0x" >> hexadecimal :: Parsec Void String Integer
let input = "0xA 000 0xFFFF"
```

### Parsing with `sepCap` family of parser combinators

Separate the input string into sections which can be parsed as a hexadecimal
number with a prefix `"0x"`, and sections which can't.

```haskell
parseTest (sepCap hexparser) input
```
```haskell
[Right 10,Left " 000 ",Right 65535]
```

Just get the strings sections which match the hexadecimal parser, throw away
the parsed number.

```haskell
parseTest (findAll hexparser) input
```
```haskell
[Right "0xA",Left " 000 ",Right "0xFFFF"]
```

Capture the parsed hexadecimal number, as well as the string section which
parses as a hexadecimal number.

```haskell
parseTest (findAllCap hexparser) input
```
```haskell
[Right ("0xA",10),Left " 000 ",Right ("0xFFFF",65535)]
```

List the offset locations of every whitespace pattern.

```haskell
import Data.Either
let spaceoffset = getOffset <* space1 :: Parsec Void String Int
parseTest (return . rights =<< sepCap spaceoffset) " a  b  "
```
```haskell
[0,2,5]
```

### Edit streams by running parsers with `streamEdit`

Find all of the string sections *`s`* which can be parsed as a
hexadecimal number *`r`*,
and if *`r≤16`*, then replace *`s`* with a decimal number.

```haskell
streamEdit (match hexparser) (\(s,r) -> if r <= 16 then show r else s) input
```
```haskell
"10 000 0xFFFF"
```

Replace all carriage-return-newline instances with newline.

```haskell
streamEdit crlf (const "\n") "1\r\n\r\n2"
```
```haskell
"1\n\n2"
```

Replace alphabetic characters with the next character in the alphabet.

```haskell
streamEdit (some letterChar) (fmap succ) "HAL 9000"
```
```haskell
"IBM 9000"
```

## Alternatives

<http://hackage.haskell.org/package/regex>



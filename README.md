# replace-megaparsec

[![Hackage](https://img.shields.io/hackage/v/replace-megaparsec.svg?style=flat)](https://hackage.haskell.org/package/replace-megaparsec)
[![Stackage Nightly](http://stackage.org/package/replace-megaparsec/badge/nightly)](http://stackage.org/nightly/package/replace-megaparsec)
[![Stackage LTS](http://stackage.org/package/replace-megaparsec/badge/lts)](http://stackage.org/lts/package/replace-megaparsec)

__replace-megaparsec__ is for finding text patterns, and also editing and
replacing the found patterns.
This activity is traditionally done with regular expressions,
but __replace-megaparsec__ uses
[__Megaparsec__](http://hackage.haskell.org/package/megaparsec)
parsers instead for the pattern matching.

__replace-megaparsec__ can be used in the same sort of “pattern capture”
or “find all” situations in which one would use Python
[`re.findall`](https://docs.python.org/3/library/re.html#re.findall)
or
Perl [`m//`](https://perldoc.perl.org/functions/m.html),
or
Unix [`grep`](https://www.gnu.org/software/grep/).

__replace-megaparsec__ can be used in the same sort of “stream editing”
or “search-and-replace” situations in which one would use Python
[`re.sub`](https://docs.python.org/3/library/re.html#re.sub),
or
Perl [`s///`](https://perldoc.perl.org/functions/s.html),
or Unix
[`sed`](https://www.gnu.org/software/sed/manual/html_node/The-_0022s_0022-Command.html),
or
[`awk`](https://www.gnu.org/software/gawk/manual/gawk.html).

## Why would we want to do pattern matching and substitution with parsers instead of regular expressions?

* Haskell parsers have a nicer syntax than
  [regular expressions](https://en.wikipedia.org/wiki/Regular_expression),
  which are notoriously
  [difficult to read](https://en.wikipedia.org/wiki/Write-only_language).

* Regular expressions can do “group capture” on sections of the matched
  pattern, but they can only return stringy lists of the capture groups. Parsers
  can construct typed data structures based on the capture groups, guaranteeing
  no disagreement between the pattern rules and the rules that we're using
  to build data structures based on the pattern matches.

  For example, consider
  scanning a string for numbers. A lot of different things can look like a number,
  and can have leading plus or minus signs, or be in scientific notation, or
  have commas, or whatever. If we try to parse all of the numbers out of a string
  using regular expressions, then we have to make sure that the regular expression
  and the string-to-number conversion function agree about exactly what is
  and what isn't a numeric string. We can get into an awkward situation in which
  the regular expression says it has found a numeric string but the
  string-to-number conversion function fails. A typed parser will perform both
  the pattern match and the conversion, so it will never be in that situation.

* Regular expressions are only able to pattern-match
  [regular](https://en.wikipedia.org/wiki/Chomsky_hierarchy#The_hierarchy)
  grammers.
  Parsers are able pattern-match with context-free grammers, and
  even context-sensitive grammers, if needed. See below for
  an example of lifting a `Parser` into a `State` monad for context-sensitive
  pattern-matching.

  The replacement expression for a traditional regular expression-based
  substitution command is usually just a simple string template in which
  the *Nth* “capture group” can be inserted with the syntax `\N`. With
  this library, instead of a template, we get
  an `editor` function which can perform any computation, including IO.

## Examples

Try the examples in `ghci` by
running `cabal v2-repl` in the `replace-megaparsec/`
root directory.

The examples depend on these imports.

```haskell
import Replace.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
```

### Parsing with `sepCap` family of parser combinators

The following examples show how to match a pattern to a string of text
and deconstruct the string of text by separating it into sections
which match the pattern, and sections which don't match.

#### Pattern match, capture only the parsed result with `sepCap`

Separate the input string into sections which can be parsed as a hexadecimal
number with a prefix `"0x"`, and sections which can't.

```haskell
let hexparser = chunk "0x" >> hexadecimal :: Parsec Void String Integer
parseTest (sepCap hexparser) "0xA 000 0xFFFF"
```
```haskell
[Right 10,Left " 000 ",Right 65535]
```

#### Pattern match, capture only the matched text with `findAll`

Just get the strings sections which match the hexadecimal parser, throw away
the parsed number.

```haskell
let hexparser = chunk "0x" >> hexadecimal :: Parsec Void String Integer
parseTest (findAll hexparser) "0xA 000 0xFFFF"
```
```haskell
[Right "0xA",Left " 000 ",Right "0xFFFF"]
```

#### Pattern match, capture the matched text and the parsed result with `findAllCap`

Capture the parsed hexadecimal number, as well as the string section which
parses as a hexadecimal number.

```haskell
let hexparser = chunk "0x" >> hexadecimal :: Parsec Void String Integer
parseTest (findAllCap hexparser) "0xA 000 0xFFFF"
```
```haskell
[Right ("0xA",10),Left " 000 ",Right ("0xFFFF",65535)]
```

#### Pattern match, capture only the locations of the matched patterns

Find all of the sections of the stream which match
the `Text.Megaparsec.Char.space1` parser (a string of whitespace).
Print a list of the offsets of the beginning of every pattern match.

```haskell
import Data.Either
let spaceoffset = getOffset <* space1 :: Parsec Void String Int
parseTest (return . rights =<< sepCap spaceoffset) " a  b  "
```
```haskell
[0,2,5]
```

### Edit text strings by running parsers with `streamEdit`

The following examples show how to search for a pattern in a string of text
and then edit the string of text to substitute in some replacement text
for the matched patterns.

#### Pattern match and replace with a constant

Replace all carriage-return-newline instances with newline.

```haskell
streamEdit (chunk "\r\n") (const "\n") "1\r\n2\r\n"
```
```haskell
"1\n2\n"
```

#### Pattern match and edit the matches

Replace alphabetic characters with the next character in the alphabet.

```haskell
streamEdit (some letterChar) (fmap succ) "HAL 9000"
```
```haskell
"IBM 9000"
```

#### Pattern match and maybe edit the matches, or maybe leave them alone

Find all of the string sections *`s`* which can be parsed as a
hexadecimal number *`r`*,
and if *`r≤16`*, then replace *`s`* with a decimal number. Uses the
[`match`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec.html#v:match)
combinator.

```haskell
let hexparser = chunk "0x" >> hexadecimal :: Parsec Void String Integer
streamEdit (match hexparser) (\(s,r) -> if r <= 16 then show r else s) "0xA 000 0xFFFF"
```
```haskell
"10 000 0xFFFF"
```

#### Context-sensitive pattern match and edit the matches

Capitalize the third letter in a string. The `capthird` parser searches for
individual letters, and it needs to remember how many times it has run so
that it can match successfully only on the third time that it finds a letter.
To enable the parser to remember how many times it has run, we'll
compose the parser with a `State` monad from
the `mtl` package. (Run in `ghci` with `cabal v2-repl -b mtl`).

```haskell
import qualified Control.Monad.State.Strict as MTL
import Control.Monad.State.Strict (get, put, evalState)
import Data.Char (toUpper)

let capthird :: ParsecT Void String (MTL.State Int) String
    capthird = do
        x <- letterChar
        i <- get
        put (i+1)
        if i==3 then return [x] else empty

flip evalState 1 $ streamEditT capthird (return . fmap toUpper) "a a a a a"
```
```haskell
"a a A a a"
```

## Alternatives

<http://hackage.haskell.org/package/regex>

<http://hackage.haskell.org/package/pipes-parse>

<http://hackage.haskell.org/package/stringsearch>

<http://hackage.haskell.org/package/substring-parser>

<http://hackage.haskell.org/package/pcre-utils>

<http://hackage.haskell.org/package/template>

<http://hackage.haskell.org/package/regex-applicative>

## Motivation

I wanted to scan a Markdown document and find tokens inside backticks that
look like a Haskell identifier, then look up the identifier in Hoogle to
see if it has a definition in __base__, and if so, insert a Hackage link
for the identifier into the Markdown. I couldn't find a simple and
obvious way to do that with any existing technology.

## Hypothetically Asked Questions

1. *Is it fast?*

   lol not really. `sepCap` is fundamentally about consuming the stream one
   token at a time while we try and fail to run a parser and then
   backtrack each time. That's
   [a slow activity](https://markkarpov.com/megaparsec/megaparsec.html#writing-efficient-parsers).

2. *Could we write this library for __Parsec__?*

   No, because the
   [`match`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec.html#v:match)
   combinator doesn't exist for Parsec. (I can't find it anywhere.
   [Can it be written?](http://www.serpentine.com/blog/2014/05/31/attoparsec/#from-strings-to-buffers-and-cursors))

3. *Could we write this library for __Attoparsec__?*

   I think so, but I wouldn't expect much of a speed improvement, because
   `sepCap` is a fundamentally slow activity.

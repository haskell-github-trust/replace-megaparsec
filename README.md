
# Parse Replace


This module is for
“pattern capture”
or
“stream editing” or “find-and-replace,” using
[__`Text.Megaparsec`__](http://hackage.haskell.org/package/megaparsec)
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

Given a parser for numbers in simple scientific notation like `"1E2"`:

    scinum :: Parsec Void String (Double, Integer)
    scinum = do
        m <- some digitChar
        string "E"
        e <- some digitChar
        return (read m, read e)

    import Data.Either
    import Data.Maybe

    let input = "1E2 xxx 2E3"

1. Parse the structure of the entire input string:

       print $ fromJust $ parseMaybe (findall scinum) input

   Entire input structure:

       [Right ("1E2",(1.0,2)), Left " xxx ", Right ("2E3",(2.0,3))]

2. Capture the parsed pattern matches:

       print $ fmap snd
             $ rights
             $ fromJust $ parseMaybe (findall scinum) input

   Parsed pattern matches:

       [(1.0,2), (2.0,3)]

3. Replace all of the matched numbers with decimal notation:

       print $ foldMap (either id (\(_,(m,e)) -> show $ m * (10 ^^ e)))
             $ fromJust $ parseMaybe (findall scinum) input

   Input string with scientific notation replaced by decimal notation:

       "100.0 xxx 2000.0"


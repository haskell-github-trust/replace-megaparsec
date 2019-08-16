-- |
-- Module    : Parsereplace.Megaparsec
--
-- This module is for
-- “pattern capture”
-- or
-- “stream editing” or “find-and-replace,” using "Text.Megaparsec" parsers
-- instead of the more traditional regular expressions.
--
-- It can be used
-- in the same sort of “pattern capture” situations in which
-- one would
-- use the Python
-- <https://docs.python.org/3/library/re.html#re.findall re.findall>
-- or Unix
-- <https://www.gnu.org/software/grep/ grep>.
--
-- This module can be used for “find-and-replace” or “stream editing” in the
-- same sort of situations in which
-- one would use Python
-- <https://docs.python.org/3/library/re.html#re.sub re.sub>
-- , or Unix
-- <https://www.gnu.org/software/sed/manual/html_node/The-_0022s_0022-Command.html sed substitute>,
-- or
-- <https://www.gnu.org/software/gawk/manual/gawk.html awk>.
--



-- Find and parse all of the non-overlapping substrings of a string which match
-- a pattern given by a parser.
--
-- This is a parser combinator which can be used for pattern capture
-- situations similar to when one would use the Python
-- [@re.findall@](https://docs.python.org/3/library/re.html#re.findall)
-- or
-- Unix [grep](https://www.gnu.org/software/grep/).
--
-- The 'findAll' function takes a pattern parser as an argument, and returns a
-- parser which will consume an entire input stream and find all the places
-- in the input stream which match the pattern.
-- The result is a list of 'Right' pattern matches and 'Left' unmatched sections
-- of the input stream.
-- The result can be examined for parsed matches, or reconstructed
-- into an edited output stream.
--
-- Examples
--
-- Given a parser for numbers in simple scientific notation like `"1E2"`:
--
--     scinum :: Parsec Void String (Double, Integer)
--     scinum = do
--         m <- some digitChar
--         string "E"
--         e <- some digitChar
--         return (read m, read e)
--
--     import Data.Either
--     import Data.Maybe
--
--     let input = "1E2 xxx 2E3"
--
-- 1. Parse the structure of the entire input string:
--
--        print $ fromJust $ parseMaybe (findall scinum) input
--
--    Entire input structure:
--
--        [Right ("1E2",(1.0,2)), Left " xxx ", Right ("2E3",(2.0,3))]
--
-- 2. Capture the parsed pattern matches:
--
--        print $ fmap snd
--              $ rights
--              $ fromJust $ parseMaybe (findall scinum) input
--
--    Parsed pattern matches:
--
--        [(1.0,2), (2.0,3)]
--
-- 3. Replace all of the matched numbers with decimal notation:
--
--        print $ foldMap (either id (\(_,(m,e)) -> show $ m * (10 ^^ e)))
--              $ fromJust $ parseMaybe (findall scinum) input
--
--    Input string with scientific notation replaced by decimal notation:
--
--        "100.0 xxx 2000.0"
--
-- Make sure we test that the parser is correctly consuming its input.
--
-- Make sure we test that the parser is correctly calculating its position.
--
-- Make sure we test that the parser will continue in event of a parse that
-- fails on an operation like `read`.
--
--
-- We need the Semigroup instance for `s` because a Megaparsec Stream has
-- methods for unconsing the Stream, but no methods for consing the Stream,
-- and findall needs to build an output stream, not just parse the input stream.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parsereplace.Megaparsec
  (
    -- * Parser combinator
    sepCap
  , findAll
  , findAllGroup

    -- * Running parser
  , streamEditT
  , streamEdit
  )
where


import Text.Megaparsec

import Data.Void
import Data.Maybe
import Data.Bifunctor
import Data.Functor.Identity
import Data.Proxy
import Data.Foldable
import Control.Exception (throw)
import Data.Typeable
import Control.Monad


-- |
-- == Separate and capture
--
-- Parser combinator to find all of the non-overlapping ocurrences
-- of the pattern @sep@ in a text stream. Separate the stream into sections:
--
-- * sections which can parsed by the pattern @sep@ will be captured as
--   matching sections in 'Right'
-- * non-matching sections of the stream will be captured in 'Left'.
--
-- This parser will always consume its entire input and can never fail.
-- If there are no pattern matches, then the entire input stream will be
-- returned as a non-matching 'Left' section.
sepCap
    :: forall e s m a. (MonadParsec e s m)
    => m a -- ^ The pattern matching parser @sep@.
    -> m [Either (Tokens s) a]
sepCap sep = (fmap.fmap) (first $ tokensToChunk (Proxy::Proxy s))
             $ fmap sequenceLeft
             $ many $ fmap Right (try sep) <|> fmap Left anySingle
  where
    sequenceLeft :: [Either l r] -> [Either [l] r]
    sequenceLeft = foldr consLeft []
      where
        consLeft :: Either l r -> [Either [l] r] -> [Either [l] r]
        consLeft (Left l) ((Left ls):xs) = (Left (l:ls)):xs
        consLeft (Left l) xs = (Left [l]):xs
        consLeft (Right r) xs = (Right r):xs

-- |
-- == Find all occurences, parse and group matches
--
-- Parser combinator for finding all occurences of a pattern in a stream.
--
-- Will calls 'sepCap' with the 'Text.Megaparsec.match' combinator so that
-- the text which matched the pattern parser @sep@ will be returned in
-- the 'Right' sections, along with the result of the parse of @sep@.
findAll
    :: MonadParsec e s m
    => m a -- ^ The pattern matching parser @sep@.
    -> m [Either (Tokens s) (Tokens s, a)]
findAll sep = sepCap (match sep)

-- |
-- == Find all occurences, group matches
--
-- Parser combinator for finding all occurences of a pattern in a stream.
--
-- Will call 'sepCap' with the 'Text.Megaparsec.match' combinator and only
-- return the text which matched the pattern parser @sep@ in
-- the 'Right' sections, rather than the result of the parse of @sep@.
findAllGroup
    :: MonadParsec e s m
    => m a -- ^ The pattern matching parser @sep@.
    -> m [Either (Tokens s) (Tokens s)]
findAllGroup sep = (fmap.fmap) (second fst) $ sepCap (match sep)


-- |
-- == Stream editor
--
-- Also can be considered “find-and-replace”. Finds all
-- of the sections of the stream which match the pattern @sep@, and replaces
-- them with the result of the @editor@ function.
--
-- This function is not a “parser combinator,” it is more like
-- an alternate “way to run a parser”, like 'Text.Megaparsec.parse'
-- or 'Text.Megaparsec.runParserT'.
--
-- The type of the stream of text that is input must
-- be @Stream s@ such that @Tokens s ~ s@, because we want
-- to output the same type of stream that was input. That requirement is
-- satisfied for all the 'Text.Megaparsec.Stream' instances included
-- with "Text.Megaparsec":
-- "Data.Text",
-- "Data.Text.Lazy",
-- "Data.Bytestring",
-- "Data.Bytestring.Lazy",
-- and "Data.String".
--
-- We also need the @Monoid s@ instance so that we can construct the output
-- stream.
--
-- We need @Typeable s@ and @Show s@ for 'Control.Exception.throw'.
--
-- === Access the original matched section
--
-- If you want access to the matched string in the @editor@ function,
-- then combine the pattern parser @sep@ with 'Text.Megaparsec.match', like
--
-- > streamEditT (match sep) (\(matchString, parseResult) -> return matchString)
--
-- === Editor context
--
-- If you want to do 'IO' operations in the @editor@ function, then run this in
-- 'IO'.
--
-- If you want the @editor@ function to remember some state, then run this in
-- a stateful 'Monad'.
--
-- === Examples
--
-- Replace all carriage-return-newline instances with newline.
--
-- > streamEdit crlf (const "\n")
--
-- Replace all numbers in scientific notation with decimal notation, but
-- only if the value of the number is smaller than 20.
streamEditT
    :: forall s m a. (Stream s, Monad m, Monoid s, Tokens s ~ s, Show s, Show (Token s), Typeable s)
    => ParsecT Void s m a
        -- ^ The parser @sep@ for the pattern of interest.
    -> (a -> m s)
        -- ^ The @editor@ function. Takes a parsed result of @sep@
        -- and returns a new stream section for the replacement.
    -> s
        -- ^ The input stream of text to be edited.
    -> m s
streamEditT sep editor input = do
    runParserT (sepCap sep) "" input >>= \case
        (Left err) -> throw err -- parser should never fail, but if it does, throw
        (Right r) -> fmap fold $ traverse (either return editor) r



-- |
-- == Pure stream editor
--
-- Pure version of 'streamEditT'.
streamEdit
    :: forall s a. (Stream s, Monoid s, Tokens s ~ s, Show s, Show (Token s), Typeable s)
    => Parsec Void s a
        -- ^ The parser @sep@ for the pattern of interest.
    -> (a -> s)
        -- ^ The @editor@ function. Takes a parsed result of @sep@
        -- and returns a new stream section for the replacement.
    -> s
        -- ^ The input stream of text to be edited.
    -> s
streamEdit sep editor = runIdentity . streamEditT sep (Identity . editor)


-- HAQ (Hypothetically Asked Questions)
--
-- Q: Is it fast?
--
-- A: Meh. (benchmark comparison to sed).


-- attoparsec has match
-- http://hackage.haskell.org/package/attoparsec-0.13.2.2/docs/Data-Attoparsec-ByteString.html#v:match
--
-- attoparsec has a Monoid instance for Chunk
-- http://hackage.haskell.org/package/attoparsec-0.13.2.2/docs/Data-Attoparsec-Types.html#t:Chunk
--
-- but attoparsec does not work for String. so.

-- https://stackoverflow.com/questions/18957873/haskell-parenthesis-matching-for-find-and-replace

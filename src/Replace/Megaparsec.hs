-- |
-- Module    : Replace.Megaparsec
-- Copyright : ©2019 James Brock
-- License   : BSD2
-- Maintainer: James Brock <jamesbrock@gmail.com>
--
-- __Replace.Megaparsec__ is for finding text patterns, and also
-- replacing or splitting on the found patterns.
-- This activity is traditionally done with regular expressions,
-- but __Replace.Megaparsec__ uses "Text.Megaparsec" parsers instead for
-- the pattern matching.
--
-- __Replace.Megaparsec__ can be used in the same sort of “pattern capture”
-- or “find all” situations in which one would use Python
-- <https://docs.python.org/3/library/re.html#re.findall re.findall>,
-- or Perl
-- <https://perldoc.perl.org/functions/m.html m//>,
-- or Unix
-- <https://www.gnu.org/software/grep/ grep>.
--
-- __Replace.Megaparsec__ can be used in the same sort of “stream editing”
-- or “search-and-replace” situations in which one would use Python
-- <https://docs.python.org/3/library/re.html#re.sub re.sub>,
-- or Perl
-- <https://perldoc.perl.org/functions/s.html s///>,
-- or Unix
-- <https://www.gnu.org/software/sed/manual/html_node/The-_0022s_0022-Command.html sed>,
-- or
-- <https://www.gnu.org/software/gawk/manual/gawk.html awk>.
--
-- __Replace.Megaparsec__ can be used in the same sort of “string splitting”
-- situations in which one would use Python
-- <https://docs.python.org/3/library/re.html#re.split re.split>
-- or Perl
-- <https://perldoc.perl.org/functions/split.html split>.
--
-- See the __replace-megaparsec__ package README for usage examples.
--
-- == Type constraints
--
-- === output stream type @Tokens s@ = input stream type @s@
--
-- All functions in the __Running Parser__ section require the type of the
-- stream of text that is input to be
-- @'Text.Megaparsec.Stream.Stream' s@
-- such that
-- @'Text.Megaparsec.Stream.Tokens' s ~ s@,
-- because we want to output the same type of stream that was input.
-- That requirement is satisfied for all the 'Text.Megaparsec.Stream' instances
-- included with "Text.Megaparsec":
--
-- * "Data.String"
-- * "Data.Text"
-- * "Data.Text.Lazy"
-- * "Data.ByteString"
-- * "Data.ByteString.Lazy"
--
-- === Custom error type @e@ should be 'Data.Void'
--
-- Megaparsec parsers have a custom error data component @e@. When writing parsers
-- to be used by this module, the custom error type @e@ should usually
-- be 'Data.Void', because every function in this module expects a parser
-- failure to occur on every token in a non-matching section of the input
-- stream, so parser failure error descriptions are not returned, and you'll
-- never see the custom error information.
--
-- == Special fast input types
--
-- Functions in this module will be “fast” when the input stream
-- type @s@ is:
--
-- * "Data.Text"
-- * "Data.ByteString"
--
-- We mean “fast” in the same sense as 'Text.Megaparsec.MonadParsec':
-- when returning subsections of the input stream,
-- we return slices of the input stream data, rather than constructing a list
-- of tokens and then building a new stream subsection from that list.
-- This relies on implementation details of the stream representation,
-- so there are specialization re-write rules in this module to make
-- that possible without adding new typeclasses.


{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

module Replace.Megaparsec
  (
    -- * Running parser
    --
    -- | Functions in this section are /ways to run parsers/
    -- (like 'Text.Megaparsec.runParser'). They take
    -- as arguments a @sep@ parser and some input, run the parser on the input,
    -- and return a result.
    breakCap
  , breakCapT
  , splitCap
  , splitCapT
  , streamEdit
  , streamEditT
    -- * Parser combinator
    --
    -- | Functions in this section are /parser combinators/. They take
    -- a @sep@ parser for an argument, combine @sep@ with another parser,
    -- and return a new parser.
  , anyTill
  , sepCap
  , findAll
  , findAllCap
  )
where


import Data.Bifunctor
import Data.Functor.Identity
import Data.Proxy
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.Text as T
import Text.Megaparsec
import Replace.Megaparsec.Internal.ByteString
import Replace.Megaparsec.Internal.Text


-- |
-- === Break on and capture one pattern
--
-- Find the first occurence of a pattern in a text stream, capture the found
-- pattern, and break the input text stream on the found pattern.
--
-- The 'breakCap' function is like 'Data.List.takeWhile', but can be predicated
-- beyond more than just the next one token. It's also like 'Data.Text.breakOn',
-- but the @needle@ can be a pattern instead of a constant string.
--
-- Be careful not to look too far
-- ahead; if the @sep@ parser looks to the end of the input then 'breakCap'
-- could be /O(n²)/.
--
-- The pattern parser @sep@ may match a zero-width pattern (a pattern which
-- consumes no parser input on success).
--
-- ==== Output
--
--  * @Nothing@ when no pattern match was found.
--  * @Just (prefix, parse_result, suffix)@ for the result of parsing the
--    pattern match, and the @prefix@ string before and the @suffix@ string
--    after the pattern match. @prefix@ and @suffix@ may be zero-length strings.
--
-- ==== Access the matched section of text
--
-- If you want to capture the matched string, then combine the pattern
-- parser @sep@ with 'Text.Megaparsec.match'.
--
-- With the matched string, we can reconstruct the input string.
-- For all @input@, @sep@, if
--
-- @
-- let ('Just' (prefix, (infix, _), suffix)) = breakCap ('Text.Megaparsec.match' sep) input
-- @
--
-- then
--
-- @
-- input == prefix '<>' infix '<>' suffix
-- @
breakCap
    :: forall e s a. (Ord e, Stream s, Tokens s ~ s)
    => Parsec e s a
        -- ^ The pattern matching parser @sep@
    -> s
        -- ^ The input stream of text
    -> Maybe (s, a, s)
        -- ^ Maybe (prefix, parse_result, suffix)
breakCap sep input = runIdentity $ breakCapT sep input
{-# INLINABLE breakCap #-}

-- |
-- === Break on and capture one pattern
--
-- Monad transformer version of 'breakCap'.
--
-- The parser @sep@ will run in the underlying monad context.
breakCapT
    :: forall m e s a. (Ord e, Stream s, Tokens s ~ s, Monad m)
    => ParsecT e s m a
        -- ^ The pattern matching parser @sep@
    -> s
        -- ^ The input stream of text
    -> m (Maybe (s, a, s))
        -- ^ Maybe (prefix, parse_result, suffix)
breakCapT sep input =
    runParserT pser "" input >>= \case
        (Left _) -> pure Nothing
        (Right x) -> pure $ Just x
  where
    pser = do
      (prefix, cap) <- anyTill sep
      suffix <- getInput
      pure (prefix, cap, suffix)
{-# INLINABLE breakCapT #-}




-- |
-- === Split on and capture all patterns
--
-- Find all occurences of the pattern @sep@, split the input string, capture
-- all the patterns and the splits.
--
-- The input string will be split on every leftmost non-overlapping occurence
-- of the pattern @sep@. The output list will contain
-- the parsed result of input string sections which match the @sep@ pattern
-- in 'Right', and non-matching sections in 'Left'.
--
-- 'splitCap' depends on 'sepCap', see 'sepCap' for more details.
--
-- ==== Access the matched section of text
--
-- If you want to capture the matched strings, then combine the pattern
-- parser @sep@ with 'Text.Megaparsec.match'.
--
-- With the matched strings, we can reconstruct the input string.
-- For all @input@, @sep@, if
--
-- @
-- let output = splitCap ('Text.Megaparsec.match' sep) input
-- @
--
-- then
--
-- @
-- input == 'Data.Monoid.mconcat' ('Data.Bifunctor.second' 'Data.Tuple.fst' '<$>' output)
-- @
splitCap
    :: forall e s a. (Ord e, Stream s, Tokens s ~ s)
    => Parsec e s a
        -- ^ The pattern matching parser @sep@
    -> s
        -- ^ The input stream of text
    -> [Either s a]
        -- ^ List of matching and non-matching input sections.
splitCap sep input = runIdentity $ splitCapT sep input
{-# INLINABLE splitCap #-}


-- |
-- === Split on and capture all patterns
--
-- Monad transformer version of 'splitCap'.
--
-- The parser @sep@ will run in the underlying monad context.
splitCapT
    :: forall e s m a. (Ord e, Stream s, Tokens s ~ s, Monad m)
    => ParsecT e s m a
        -- ^ The pattern matching parser @sep@
    -> s
        -- ^ The input stream of text
    -> m [Either s a]
        -- ^ List of matching and non-matching input sections.
splitCapT sep input =
    runParserT (sepCap sep) "" input >>= \case
        (Left _) -> undefined -- sepCap can never fail
        (Right r) -> pure r
{-# INLINABLE splitCapT #-}


-- |
-- === Stream editor
--
-- Also known as “find-and-replace”, or “match-and-substitute”. Finds all
-- non-overlapping sections of the stream which match the pattern @sep@,
-- and replaces them with the result of the @editor@ function.
--
-- ==== Access the matched section of text in the @editor@
--
-- If you want access to the matched string in the @editor@ function,
-- then combine the pattern parser @sep@ with 'Text.Megaparsec.match'.
-- This will effectively change the type of the @editor@ function
-- to @(s,a) -> s@.
--
-- This allows us to write an @editor@ function which can choose to not
-- edit the match and just leave it as it is. If the @editor@ function
-- returns the first item in the tuple, then @streamEdit@ will not change
-- the matched string.
--
-- So, for all @sep@:
--
-- @
-- streamEdit ('Text.Megaparsec.match' sep) 'Data.Tuple.fst' ≡ 'Data.Function.id'
-- @
streamEdit
    :: forall e s a. (Ord e, Stream s, Monoid s, Tokens s ~ s)
    => Parsec e s a
        -- ^ The pattern matching parser @sep@
    -> (a -> s)
        -- ^ The @editor@ function. Takes a parsed result of @sep@
        -- and returns a new stream section for the replacement.
    -> s
        -- ^ The input stream of text to be edited
    -> s
        -- ^ The edited input stream
streamEdit sep editor = runIdentity . streamEditT sep (Identity . editor)
{-# INLINABLE streamEdit #-}

-- |
-- === Stream editor
--
-- Monad transformer version of 'streamEdit'.
--
-- Both the parser @sep@ and the @editor@ function will run in the underlying
-- monad context.
--
-- If you want to do 'IO' operations in the @editor@ function or the
-- parser @sep@, then run this in 'IO'.
--
-- If you want the @editor@ function or the parser @sep@ to remember some state,
-- then run this in a stateful monad.
streamEditT
    :: forall e s m a. (Ord e, Stream s, Monad m, Monoid s, Tokens s ~ s)
    => ParsecT e s m a
        -- ^ The pattern matching parser @sep@
    -> (a -> m s)
        -- ^ The @editor@ function. Takes a parsed result of @sep@
        -- and returns a new stream section for the replacement.
    -> s
        -- ^ The input stream of text to be edited
    -> m s
        -- ^ The edited input stream
streamEditT sep editor input = do
    runParserT (sepCap sep) "" input >>= \case
        (Left _) -> undefined -- sepCap can never fail
        (Right r) -> mconcat <$> traverse (either return editor) r
{-# INLINABLE streamEditT #-}

-- |
-- === Specialized <http://hackage.haskell.org/package/parser-combinators/docs/Control-Monad-Combinators.html#v:manyTill_ manyTill_>
--
-- Parser combinator to consume input until the @sep@ pattern matches,
-- equivalent to
-- @'Control.Monad.Combinators.manyTill_' 'Text.Megaparsec.anySingle' sep@.
-- On success, returns the prefix before the pattern match and the parsed match.
--
-- @sep@ may be a zero-width parser, it may succeed without consuming any
-- input.
--
-- This combinator will produce a parser which
-- acts like 'Text.Megaparsec.takeWhileP' but is predicated beyond more than
-- just the next one token. 'anyTill' is also like 'Text.Megaparsec.takeWhileP'
-- in that it will be “fast” when applied to an input stream type @s@
-- for which there are specialization re-write rules.
anyTill
    :: forall e s m a. (MonadParsec e s m)
    => m a -- ^ The pattern matching parser @sep@
    -> m (Tokens s, a) -- ^ parser
anyTill sep = do
    (as, end) <- manyTill_ anySingle sep
    pure (tokensToChunk (Proxy::Proxy s) as, end)
{-# INLINE [1] anyTill #-}
#if MIN_VERSION_GLASGOW_HASKELL(8,8,1,0)
{-# RULES "anyTill/ByteString" [2]
 forall e. forall.
 anyTill           @e @B.ByteString =
 anyTillByteString @e @B.ByteString #-}
{-# RULES "anyTill/Text" [2]
 forall e. forall.
 anyTill     @e @T.Text =
 anyTillText @e @T.Text #-}
#elif MIN_VERSION_GLASGOW_HASKELL(8,0,2,0)
{-# RULES "anyTill/ByteString" [2]
 forall (pa :: ParsecT e B.ByteString m a).
 anyTill           @e @B.ByteString @(ParsecT e B.ByteString m) @a pa =
 anyTillByteString @e @B.ByteString @(ParsecT e B.ByteString m) @a pa #-}
{-# RULES "anyTill/Text" [2]
 forall (pa :: ParsecT e T.Text m a).
 anyTill     @e @T.Text @(ParsecT e T.Text m) @a pa =
 anyTillText @e @T.Text @(ParsecT e T.Text m) @a pa #-}
#endif

-- |
-- === Separate and capture
--
-- Parser combinator to find all of the leftmost non-overlapping occurrences
-- of the pattern parser @sep@ in a text stream.
-- The 'sepCap' parser will always consume its entire input and can never fail.
--
-- @sepCap@ is similar to the @sep*@ family of parser combinators
-- found in
-- <http://hackage.haskell.org/package/parser-combinators/docs/Control-Monad-Combinators.html parser-combinators>
-- and
-- <http://hackage.haskell.org/package/parsers/docs/Text-Parser-Combinators.html parsers>,
-- but it returns the parsed result of the @sep@ parser instead
-- of throwing it away.
--
-- ==== Output
--
-- The input stream is separated and output into a list of sections:
--
-- * Sections which can parsed by the pattern @sep@ will be parsed and captured
--   as 'Right'.
-- * Non-matching sections of the stream will be captured in 'Left'.
--
-- The output list also has these properties:
--
-- * If the input is @""@ then the output list will be @[]@.
-- * If there are no pattern matches, then
--   the entire input stream will be returned as one non-matching 'Left' section.
-- * The output list will not contain two consecutive 'Left' sections.
--
-- ==== Zero-width matches forbidden
--
-- If the pattern matching parser @sep@ would succeed without consuming any
-- input then 'sepCap' will force it to fail.
-- If we allow @sep@ to match a zero-width pattern,
-- then it can match the same zero-width pattern again at the same position
-- on the next iteration, which would result in an infinite number of
-- overlapping pattern matches.
sepCap
    :: forall e s m a. (MonadParsec e s m)
    => m a -- ^ The pattern matching parser @sep@
    -> m [Either (Tokens s) a] -- ^ parser
sepCap sep = (fmap.fmap) (first $ tokensToChunk (Proxy::Proxy s))
             $ fmap sequenceLeft
             $ many $ fmap Right (try nonZeroSep) <|> fmap Left anySingle
  where
    sequenceLeft :: [Either l r] -> [Either [l] r]
    sequenceLeft = {-# SCC sequenceLeft #-} foldr consLeft []
      where
        consLeft :: Either l r -> [Either [l] r] -> [Either [l] r]
        consLeft (Left l) ((Left ls):xs) = {-# SCC consLeft #-} (Left (l:ls)):xs
        consLeft (Left l) xs = {-# SCC consLeft #-} (Left [l]):xs
        consLeft (Right r) xs = {-# SCC consLeft #-} (Right r):xs
    -- If sep succeeds and consumes 0 input tokens, we must force it to fail,
    -- otherwise infinite loop
    nonZeroSep = {-# SCC nonZeroSep #-} do
        offset1 <- getOffset
        x <- {-# SCC sep #-} sep
        offset2 <- getOffset
        when (offset1 >= offset2) empty
        return x
{-# INLINE [1] sepCap #-}
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#specialisation
-- What we're missing here is a rule that can pick up non-ParsecT instances
-- of MonadParsec for GHC < 8.8.
#if MIN_VERSION_GLASGOW_HASKELL(8,8,1,0)
{-# RULES "sepCap/ByteString" [2]
 forall e. forall.
 sepCap           @e @B.ByteString =
 sepCapByteString @e @B.ByteString #-}
{-# RULES "sepCap/Text" [2]
 forall e. forall.
 sepCap     @e @T.Text =
 sepCapText @e @T.Text #-}
#elif MIN_VERSION_GLASGOW_HASKELL(8,0,2,0)
{-# RULES "sepCap/ByteString" [2]
 forall (pa :: ParsecT e B.ByteString m a).
 sepCap           @e @B.ByteString @(ParsecT e B.ByteString m) @a pa =
 sepCapByteString @e @B.ByteString @(ParsecT e B.ByteString m) @a pa #-}
{-# RULES "sepCap/Text" [2]
 forall (pa :: ParsecT e T.Text m a).
 sepCap     @e @T.Text @(ParsecT e T.Text m) @a pa =
 sepCapText @e @T.Text @(ParsecT e T.Text m) @a pa #-}
#endif


-- |
-- === Find all occurences, parse and capture pattern matches
--
-- Parser combinator for finding all occurences of a pattern in a stream.
--
-- Will call 'sepCap' with the 'Text.Megaparsec.match' combinator so that
-- the text which matched the pattern parser @sep@ will be returned in
-- the 'Right' sections, along with the result of the parse of @sep@.
--
-- Definition:
--
-- @
-- findAllCap sep = 'sepCap' ('Text.Megaparsec.match' sep)
-- @
findAllCap
    :: MonadParsec e s m
    => m a -- ^ The pattern matching parser @sep@
    -> m [Either (Tokens s) (Tokens s, a)] -- ^ parser
findAllCap sep = sepCap (match sep)
{-# INLINABLE findAllCap #-}
{-# DEPRECATED findAllCap "replace with `findAllCap sep = sepCap (match sep)`" #-}


-- |
-- === Find all occurences
--
-- Parser combinator for finding all occurences of a pattern in a stream.
--
-- Will call 'sepCap' with the 'Text.Megaparsec.match' combinator and
-- return the text which matched the pattern parser @sep@ in
-- the 'Right' sections.
--
-- Definition:
--
-- @
-- findAll sep = (fmap.fmap) ('Data.Bifunctor.second' fst) $ 'sepCap' ('Text.Megaparsec.match' sep)
-- @
findAll
    :: MonadParsec e s m
    => m a -- ^ The pattern matching parser @sep@
    -> m [Either (Tokens s) (Tokens s)] -- ^ parser
findAll sep = (fmap.fmap) (second fst) $ sepCap (match sep)
{-# INLINABLE findAll #-}
{-# DEPRECATED findAll "replace with `findAll sep = (fmap.fmap) (second fst) $ sepCap (match sep)`" #-}

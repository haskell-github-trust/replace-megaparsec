-- |
-- Module    : Replace.Megaparsec.Internal.ByteString
-- Copyright : ©2019 James Brock
-- License   : BSD2
-- Maintainer: James Brock <jamesbrock@gmail.com>
--
-- This internal module is for 'Data.ByteString.ByteString' specializations.
--
-- The functions in this module are intended to be chosen automatically
-- by rewrite rules in the "Replace.Megaparsec" module, so you should never
-- need to import this module.
--
-- Names in this module may change without a major version increment.

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Replace.Megaparsec.Internal.ByteString
  (
    -- * Parser combinator
    sepCapByteString
  , anyTillByteString
  )
where

import Control.Monad
import qualified Data.ByteString as B
import Text.Megaparsec

{-# INLINE [1] sepCapByteString #-}
sepCapByteString
    :: forall e s m a. (MonadParsec e s m, s ~ B.ByteString)
    => m a -- ^ The pattern matching parser @sep@
    -> m [Either (Tokens s) a]
sepCapByteString sep = getInput >>= go
  where
    -- the go function will search for the first pattern match,
    -- and then capture the pattern match along with the preceding
    -- unmatched string, and then recurse.
    -- restBegin is the rest of the buffer after the last pattern
    -- match.
    go restBegin = do
        (<|>)
            ( do
                restThis <- getInput
                -- About 'thisiter':
                -- It looks stupid and introduces a completely unnecessary
                -- Maybe, but when I refactor to eliminate 'thisiter' and
                -- the Maybe then the benchmarks get dramatically worse.
                thisiter <- (<|>)
                    ( do
                        x <- try sep
                        restAfter <- getInput
                        -- Don't allow a match of a zero-width pattern
                        when (B.length restAfter >= B.length restThis) empty
                        pure $ Just (x, restAfter)
                    )
                    (anySingle >> pure Nothing)
                case thisiter of
                    (Just (x, restAfter)) | B.length restThis < B.length restBegin -> do
                        -- we've got a match with some preceding unmatched string
                        let unmatched = B.take (B.length restBegin - B.length restThis) restBegin
                        (Left unmatched:) <$> (Right x:) <$> go restAfter
                    (Just (x, restAfter)) -> do
                        -- we're got a match with no preceding unmatched string
                        (Right x:) <$> go restAfter
                    Nothing -> go restBegin -- no match, try again
            )
            ( do
                -- We're at the end of the input, so return
                -- whatever unmatched string we've got since offsetBegin
                if B.length restBegin > 0 then
                    pure [Left restBegin]
                else pure []
            )

{-# INLINE [1] anyTillByteString #-}
anyTillByteString
    :: forall e s m a. (MonadParsec e s m, s ~ B.ByteString)
    => m a -- ^ The pattern matching parser @sep@
    -> m (Tokens s, a)
anyTillByteString sep = do
    begin <- getInput
    (end, x) <- go
    pure (B.take (B.length begin - B.length end) begin, x)
  where
    go = do
      end <- getInput
      r <- optional $ try sep
      case r of
        Nothing -> anySingle >> go
        Just x -> pure (end, x)

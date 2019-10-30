-- |
-- Module    : Replace.Megaparsec.Internal.Text
-- Copyright : ©2019 James Brock
-- License   : BSD2
-- Maintainer: James Brock <jamesbrock@gmail.com>
--
-- This internal module is for 'Data.ByteString.ByteString' specializations.
--
-- The functions in this module are supposed to be chosen automatically
-- by rewrite rules in the "Replace.Megaparsec" module, so you should never
-- need to import this module.

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Replace.Megaparsec.Internal.Text
  (
    -- * Parser combinator
    sepCapText
  )
where

import Control.Monad
import qualified Data.Text as T
import Text.Megaparsec

{-# INLINE [1] sepCapText #-}
sepCapText
    :: forall e s m a. (MonadParsec e s m, s ~ T.Text)
    => m a -- ^ The pattern matching parser @sep@
    -> m [Either (Tokens s) a]
sepCapText sep = getInput >>= go
  where
    -- the go function will search for the first pattern match,
    -- and then capture the pattern match along with the preceding
    -- unmatched string, and then recurse.
    -- restBegin is the rest of the buffer after the last pattern
    -- match.
    go restBegin = do
        -- !offsetThis <- getOffset
        (<|>)
            ( do
                restThis <- getInput
                -- About 'thisiter':
                -- It looks stupid and introduces a completely unnecessary
                -- Maybe, but when I refactor to eliminate 'thisiter' and
                -- the Maybe then the benchmarks get dramatically worse.
                thisiter <- (<|>)
                    ( do
                        x <- sep
                        -- !offsetAfter <- getOffset
                        restAfter <- getInput
                        -- Don't allow a match of a zero-width pattern
                        when (T.length restAfter >= T.length restThis) empty
                        return $ Just (x, restAfter)
                    )
                    (anySingle >> return Nothing)
                case thisiter of
                    (Just (x, restAfter)) | T.length restThis < T.length restBegin -> do
                        -- we've got a match with some preceding unmatched string
                        let unmatched = T.take (T.length restBegin - T.length restThis) restBegin
                        -- unmatched <- substring offsetBegin offsetThis
                        (Left unmatched:) <$> (Right x:) <$> go restAfter
                    (Just (x, restAfter)) -> do
                        -- we're got a match with no preceding unmatched string
                        (Right x:) <$> go restAfter
                    Nothing -> go restBegin -- no match, try again
            )
            ( do
                if T.length restBegin > 0 then
                    -- If we're at the end of the input, then return
                    -- whatever unmatched string we've got since offsetBegin
                    -- substring offsetBegin offsetThis >>= \s -> pure [Left s]
                    pure [Left restBegin]
                else pure []
            )


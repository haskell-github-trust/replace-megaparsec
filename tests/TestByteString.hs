{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}

module TestByteString ( tests ) where

import Replace.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer
import Data.Void
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import GHC.Word
import Test.Hspec (describe, shouldBe, it, SpecWith)

type Parser = Parsec Void B.ByteString

findAllCap' :: MonadParsec e s m => m a -> m [Either (Tokens s) (Tokens s, a)]
findAllCap' sep = sepCap (match sep)

tests :: SpecWith ()
tests = describe "input ByteString" do
    runParserTest "findAll upperChar"
        (findAllCap' (upperChar :: Parser Word8))
        ("aBcD" :: B.ByteString)
        [Left "a", Right ("B", c2w 'B'), Left "c", Right ("D", c2w 'D')]
    -- check that sepCap can progress even when parser consumes nothing
    -- and succeeds.
    runParserTest "zero-consumption parser"
        (sepCap (many (upperChar :: Parser Word8)))
        ("aBcD" :: B.ByteString)
        [Left "a", Right [c2w 'B'], Left "c", Right [c2w 'D']]
    runParserTest "scinum"
        (sepCap scinum)
        "1E3"
        [Right (1,3)]
    runParserTest "getOffset"
        (sepCap offsetA)
        "xxAxx"
        [Left "xx", Right 2, Left "xx"]
    runParserTest "monad fail"
        (sepCap (fail "" :: Parser ()))
        "xxx"
        [Left "xxx"]
#if MIN_VERSION_GLASGOW_HASKELL(8,6,0,0)
    runParserTest "read fail"
        (sepCap (return (read "a" :: Int) :: Parser Int))
        "a"
        [Left "a"]
#endif
    runParserTest "empty input" (sepCap (fail "" :: Parser ())) "" []
    streamEditTest "x to o"
        (string "x" :: Parser B.ByteString) (const "o")
        "x x x" "o o o"
    streamEditTest "x to o inner"
        (string "x" :: Parser B.ByteString) (const "o")
        " x x x " " o o o "
    streamEditTest "ordering"
        (string "456" :: Parser B.ByteString) (const "ABC")
        "123456789" "123ABC789"
    streamEditTest "empty input" (match (fail "" :: Parser ())) fst "" ""
    breakCapTest "basic" (upperChar :: Parser Word8) "aAa" (Just ("a", c2w 'A', "a"))
    breakCapTest "first" (upperChar :: Parser Word8) "Aa" (Just ("", c2w 'A', "a"))
    breakCapTest "last" (upperChar :: Parser Word8) "aA" (Just ("a", c2w 'A', ""))
    breakCapTest "fail" (upperChar :: Parser Word8) "aaa" Nothing
    breakCapTest "match" (match (upperChar :: Parser Word8)) "aAa" (Just ("a", ("A",c2w 'A'), "a"))
    breakCapTest "zero-width" (lookAhead (upperChar :: Parser Word8)) "aAa" (Just ("a", c2w 'A', "Aa"))
    breakCapTest "empty input" (upperChar :: Parser Word8) "" Nothing
    breakCapTest "empty input zero-width" (return () :: Parser ()) "" (Just ("", (), ""))
  where
    runParserTest nam p input expected = it nam $ shouldBe
        (runParser p "" input) (Right expected)

    streamEditTest nam sep editor input expected = it nam $ shouldBe
        (streamEdit sep editor input) expected

    breakCapTest nam sep input expected = it nam $ shouldBe
        (breakCap sep input) expected

    scinum :: Parser (Double, Integer)
    scinum = do
        -- This won't parse mantissas that contain a decimal point,
        -- but if we use the Text.Megaparsec.Byte.Lexer.float, then it consumes
        -- the "E" and the exponent. Whatever, doesn't really matter for this test.
        m <- (fromIntegral :: Integer -> Double) <$> decimal
        _ <- chunk "E"
        e <- decimal
        return (m, e)

    offsetA :: Parser Int
    offsetA = getOffset <* chunk "A"


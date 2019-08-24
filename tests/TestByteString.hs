{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TestByteString ( tests ) where

import Distribution.TestSuite
import Parsereplace
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer
import Data.Void
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.ByteString.Internal (c2w)
import GHC.Word

type Parser = Parsec Void ByteString

tests :: IO [Test]
tests = return
    [ Test $ runParserTest "findAll upperChar"
        (findAllCap (upperChar :: Parser Word8))
        ("aBcD" :: ByteString)
        [Left "a", Right ("B", c2w 'B'), Left "c", Right ("D", c2w 'D')]
    -- check that sepCap can progress even when parser consumes nothing
    -- and succeeds.
    , Test $ runParserTest "zero-consumption parser"
        (sepCap (many (upperChar :: Parser Word8)))
        ("aBcD" :: ByteString)
        [Left "a", Right [c2w 'B'], Left "c", Right [c2w 'D']]
    , Test $ runParserTest "scinum"
        (sepCap scinum)
        ("1E3")
        ([Right (1,3)])
    , Test $ runParserTest "getOffset"
        (sepCap offsetA)
        ("xxAxx")
        ([Left "xx", Right 2, Left "xx"])
    , Test $ runParserTest "monad fail"
        (sepCap (fail "" :: Parser ()))
        ("xxx")
        ([Left "xxx"])
    , Test $ runParserTest "read fail"
        (sepCap (return (read "a" :: Int) :: Parser Int))
        ("a")
        ([Left "a"])
    ]

runParserTest name p input expected = TestInstance
        { run = do
            case runParser p "" input of
                Left e -> return (Finished $ Fail $ show e)
                Right output ->
                    if (output == expected)
                        then return (Finished Pass)
                        else return (Finished $ Fail
                                    $ show output ++ " ≠ " ++ show expected)
        , name = name
        , tags = []
        , options = []
        , setOption = \_ _ -> Left "no options supported"
        }

scinum :: Parser (Double, Integer)
scinum = do
    -- This won't parse mantissas that contain a decimal point,
    -- but if we use the Text.Megaparsec.Byte.Lexer.float, then it consumes
    -- the "E" and the exponent. Whatever, doesn't really matter for this test.
    m <- fromIntegral <$> decimal
    string "E"
    e <- decimal
    return (m, e)


offsetA :: Parser Int
offsetA = getOffset <* string "A"


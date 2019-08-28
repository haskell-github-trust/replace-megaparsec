{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TestText ( tests ) where

import Distribution.TestSuite
import Replace.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void
import qualified Data.Text as T
import Data.Text (Text)

type Parser = Parsec Void Text

tests :: IO [Test]
tests = return
    [ Test $ runParserTest "findAll upperChar"
        (findAllCap (upperChar :: Parser Char))
        ("aBcD" :: Text)
        [Left "a", Right ("B", 'B'), Left "c", Right ("D", 'D')]
    -- check that sepCap can progress even when parser consumes nothing
    -- and succeeds.
    , Test $ runParserTest "zero-consumption parser"
        (sepCap (many (upperChar :: Parser Char)))
        ("aBcD" :: Text)
        [Left "a", Right "B", Left "c", Right "D"]
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
    m <- some digitChar
    chunk "E"
    e <- some digitChar
    return (read m, read e)


offsetA :: Parser Int
offsetA = getOffset <* chunk "A"


{-# LANGUAGE FlexibleContexts #-}

module Tests ( tests ) where

import Distribution.TestSuite
import Parsereplace
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

tests :: IO [Test]
tests = return
    [ Test $ runParserTest "findAll upperChar"
        (findAll (upperChar :: Parser Char))
        ("aBcD" :: String)
        [Left "a", Right ("B", 'B'), Left "c", Right ("D", 'D')]
    , Test $ runParserTest "zero-consumption parser"
        (sepCap (many (upperChar :: Parser Char)))
        ("aBcD" :: String)
        [Left "a", Right "B", Left "c", Right "D"]
    ]

runParserTest name p input expected = TestInstance
        { run = do
            case runParser p "" input of
                Left e -> return (Finished $ Fail $ show e)
                Right output ->
                    if (output == expected)
                        then return (Finished Pass)
                        else return (Finished $ Fail
                                    $ show expected ++ " ≠ " ++ show output)
        , name = name
        , tags = []
        , options = []
        , setOption = \_ _ -> Left "no options supported"
        }

-- streamEdit (many lowerChar) (fmap toUpper) "as12df"

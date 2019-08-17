module Tests ( tests ) where

import Distribution.TestSuite
import Parsereplace
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

tests :: IO [Test]
tests = return
    [ Test $ findAllTest
        (upperChar :: Parser Char)
        ("aBcD" :: String)
        [Left "a", Right ("B", 'B'), Left "c", Right ("D", 'D')]
    ]

findAllTest sep input expected = TestInstance
        { run = do
            case runParser (findAll sep) "" input of
                Left e -> return (Finished $ Fail $ show e)
                Right output ->
                    if (output == expected)
                        then return (Finished Pass)
                        else return (Finished $ Fail
                                    $ show expected ++ " ≠ " ++ show output)
        , name = "findAllTest " ++ input
        , tags = []
        , options = []
        , setOption = \_ _ -> Left "no options supported"
        }

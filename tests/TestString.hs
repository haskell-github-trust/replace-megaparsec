{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module TestString ( tests ) where

import Distribution.TestSuite
import Distribution.TestSuite as TestSuite
import Replace.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

tests :: IO [Test]
tests = return
    [ Test $ runParserTest "findAll upperChar"
        (findAllCap (upperChar :: Parser Char))
        ("aBcD" :: String)
        [Left "a", Right ("B", 'B'), Left "c", Right ("D", 'D')]
    -- check that sepCap can progress even when parser consumes nothing
    -- and succeeds.
    , Test $ runParserTest "zero-consumption parser"
        (sepCap (many (upperChar :: Parser Char)))
        ("aBcD" :: String)
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
    , Test $ runParserTest "findAll astral"
        (findAll ((takeWhileP Nothing (=='𝅘𝅥𝅯') :: Parser String)))
        ("𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥")
        [Left "𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥", Right "𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯", Left "𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥"]
    , Test $ streamEditTest "x to o"
        (string "x" :: Parser String) (const "o")
        "x x x" "o o o"
    , Test $ streamEditTest "x to o inner"
        (string "x" :: Parser String) (const "o")
        " x x x " " o o o "
    , Test $ streamEditTest "ordering"
        (string "456" :: Parser String) (const "ABC")
        "123456789" "123ABC789"
    ]
  where
    runParserTest nam p input expected = TestInstance
            { run = do
                case runParser p "" input of
                    Left e -> return (Finished $ Fail $ show e)
                    Right output ->
                        if (output == expected)
                            then return (Finished Pass)
                            else return (Finished $ Fail
                                        $ show output ++ " ≠ " ++ show expected)
            , name = nam
            , tags = []
            , options = []
            , setOption = \_ _ -> Left "no options supported"
            }

    streamEditTest nam sep editor input expected = TestInstance
            { run = do
                let output = streamEdit sep editor input
                if (output == expected)
                    then return (Finished Pass)
                    else return (Finished $ TestSuite.Fail
                                $ show output ++ " ≠ " ++ show expected)
            , name = "streamEdit " ++ nam
            , tags = []
            , options = []
            , setOption = \_ _ -> Left "no options supported"
            }

    scinum :: Parser (Double, Integer)
    scinum = do
        m <- some digitChar
        _ <- chunk "E"
        e <- some digitChar
        return (read m, read e)


    offsetA :: Parser Int
    offsetA = getOffset <* chunk "A"


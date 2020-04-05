{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

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
#if MIN_VERSION_GLASGOW_HASKELL(8,6,0,0)
    , Test $ runParserTest "read fail"
        (sepCap (return (read "a" :: Int) :: Parser Int))
        ("a")
        ([Left "a"])
#endif
    , Test $ runParserTest "findAll astral"
        (findAll ((takeWhileP Nothing (=='𝅘𝅥𝅯') :: Parser String)))
        ("𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥")
        [Left "𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥", Right "𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯", Left "𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥"]
    , Test $ runParserTest "empty input" (sepCap (fail "" :: Parser ())) "" []
    , Test $ streamEditTest "x to o"
        (string "x" :: Parser String) (const "o")
        "x x x" "o o o"
    , Test $ streamEditTest "x to o inner"
        (string "x" :: Parser String) (const "o")
        " x x x " " o o o "
    , Test $ streamEditTest "ordering"
        (string "456" :: Parser String) (const "ABC")
        "123456789" "123ABC789"
    , Test $ streamEditTest "empty input" (match (fail "" :: Parser ())) (fst) "" ""
    , Test $ splitCapTest "basic" (upperChar :: Parser Char) "aAa" (Just ("a", 'A', "a"))
    , Test $ splitCapTest "first" (upperChar :: Parser Char) "Aa" (Just ("", 'A', "a"))
    , Test $ splitCapTest "last" (upperChar :: Parser Char) "aA" (Just ("a", 'A', ""))
    , Test $ splitCapTest "fail" (upperChar :: Parser Char) "aaa" Nothing
    , Test $ splitCapTest "match" (match (upperChar :: Parser Char)) "aAa" (Just ("a", ("A",'A'), "a"))
    , Test $ splitCapTest "zero-width" (lookAhead (upperChar :: Parser Char)) "aAa" (Just ("a", 'A', "Aa"))
    , Test $ splitCapTest "empty input" (upperChar :: Parser Char) "" Nothing
    , Test $ splitCapTest "empty input zero-width" (return () :: Parser ()) "" (Just ("", (), ""))
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

    splitCapTest nam sep input expected = TestInstance
            { run = do
                let output = splitCap sep input
                if (output == expected)
                    then return (Finished Pass)
                    else return (Finished $ TestSuite.Fail
                                $ show output ++ " ≠ " ++ show expected)
            , name = "splitCap " ++ nam
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


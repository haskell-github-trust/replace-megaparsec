{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}

module TestString ( tests ) where

import Replace.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Bifunctor (second)
import Test.Hspec (describe, shouldBe, it, SpecWith)

type Parser = Parsec Void String

findAllCap' :: MonadParsec e s m => m a -> m [Either (Tokens s) (Tokens s, a)]
findAllCap' sep = sepCap (match sep)

findAll' :: MonadParsec e s f => f b -> f [Either (Tokens s) (Tokens s)]
findAll' sep = (fmap.fmap) (second fst) $ sepCap (match sep)

tests :: SpecWith ()
tests = describe "input String" do
    runParserTest "findAll upperChar"
        (findAllCap' (upperChar :: Parser Char))
        ("aBcD" :: String)
        [Left "a", Right ("B", 'B'), Left "c", Right ("D", 'D')]
    -- check that sepCap can progress even when parser consumes nothing
    -- and succeeds.
    runParserTest "zero-consumption parser"
        (sepCap (many (upperChar :: Parser Char)))
        ("aBcD" :: String)
        [Left "a", Right "B", Left "c", Right "D"]
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
        ("a")
        ([Left "a"])
#endif
    runParserTest "findAll astral"
        (findAll' (takeWhileP Nothing (=='𝅘𝅥𝅯') :: Parser String))
        "𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥"
        [Left "𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥", Right "𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯", Left "𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥"]
    runParserTest "empty input" (sepCap (fail "" :: Parser ())) "" []
    streamEditTest "x to o"
        (string "x" :: Parser String) (const "o")
        "x x x" "o o o"
    streamEditTest "x to o inner"
        (string "x" :: Parser String) (const "o")
        " x x x " " o o o "
    streamEditTest "ordering"
        (string "456" :: Parser String) (const "ABC")
        "123456789" "123ABC789"
    streamEditTest "empty input" (match (fail "" :: Parser ())) fst "" ""
    breakCapTest "basic" (upperChar :: Parser Char) "aAa" (Just ("a", 'A', "a"))
    breakCapTest "first" (upperChar :: Parser Char) "Aa" (Just ("", 'A', "a"))
    breakCapTest "last" (upperChar :: Parser Char) "aA" (Just ("a", 'A', ""))
    breakCapTest "fail" (upperChar :: Parser Char) "aaa" Nothing
    breakCapTest "match" (match (upperChar :: Parser Char)) "aAa" (Just ("a", ("A",'A'), "a"))
    breakCapTest "zero-width" (lookAhead (upperChar :: Parser Char)) "aAa" (Just ("a", 'A', "Aa"))
    breakCapTest "empty input" (upperChar :: Parser Char) "" Nothing
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
        m <- some digitChar
        _ <- chunk "E"
        e <- some digitChar
        return (read m, read e)

    offsetA :: Parser Int
    offsetA = getOffset <* chunk "A"


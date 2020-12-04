{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module TestByteString ( tests ) where

import Distribution.TestSuite as TestSuite
import Replace.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer
import Data.Void
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import GHC.Word

type Parser = Parsec Void B.ByteString

findAllCap' :: MonadParsec e s m => m a -> m [Either (Tokens s) (Tokens s, a)]
findAllCap' sep = sepCap (match sep)

tests :: IO [Test]
tests = return
    [ Test $ runParserTest "findAll upperChar"
        (findAllCap' (upperChar :: Parser Word8))
        ("aBcD" :: B.ByteString)
        [Left "a", Right ("B", c2w 'B'), Left "c", Right ("D", c2w 'D')]
    -- check that sepCap can progress even when parser consumes nothing
    -- and succeeds.
    , Test $ runParserTest "zero-consumption parser"
        (sepCap (many (upperChar :: Parser Word8)))
        ("aBcD" :: B.ByteString)
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
#if MIN_VERSION_GLASGOW_HASKELL(8,6,0,0)
    , Test $ runParserTest "read fail"
        (sepCap (return (read "a" :: Int) :: Parser Int))
        ("a")
        ([Left "a"])
#endif
    , Test $ runParserTest "empty input" (sepCap (fail "" :: Parser ())) "" []
    , Test $ streamEditTest "x to o"
        (string "x" :: Parser B.ByteString) (const "o")
        "x x x" "o o o"
    , Test $ streamEditTest "x to o inner"
        (string "x" :: Parser B.ByteString) (const "o")
        " x x x " " o o o "
    , Test $ streamEditTest "ordering"
        (string "456" :: Parser B.ByteString) (const "ABC")
        "123456789" "123ABC789"
    , Test $ streamEditTest "empty input" (match (fail "" :: Parser ())) (fst) "" ""
    , Test $ breakCapTest "basic" (upperChar :: Parser Word8) "aAa" (Just ("a", c2w 'A', "a"))
    , Test $ breakCapTest "first" (upperChar :: Parser Word8) "Aa" (Just ("", c2w 'A', "a"))
    , Test $ breakCapTest "last" (upperChar :: Parser Word8) "aA" (Just ("a", c2w 'A', ""))
    , Test $ breakCapTest "fail" (upperChar :: Parser Word8) "aaa" Nothing
    , Test $ breakCapTest "match" (match (upperChar :: Parser Word8)) "aAa" (Just ("a", ("A",c2w 'A'), "a"))
    , Test $ breakCapTest "zero-width" (lookAhead (upperChar :: Parser Word8)) "aAa" (Just ("a", c2w 'A', "Aa"))
    , Test $ breakCapTest "empty input" (upperChar :: Parser Word8) "" Nothing
    , Test $ breakCapTest "empty input zero-width" (return () :: Parser ()) "" (Just ("", (), ""))
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
                                        $ "got " <> show output <> " expected " <> show expected)
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
                                $ "got " <> show output <> " expected " <> show expected)
            , name = "streamEdit " ++ nam
            , tags = []
            , options = []
            , setOption = \_ _ -> Left "no options supported"
            }

    breakCapTest nam sep input expected = TestInstance
            { run = do
                let output = breakCap sep input
                if (output == expected)
                    then return (Finished Pass)
                    else return (Finished $ TestSuite.Fail
                                $ "got " <> show output <> " expected " <> show expected)
            , name = "breakCap " ++ nam
            , tags = []
            , options = []
            , setOption = \_ _ -> Left "no options supported"
            }

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


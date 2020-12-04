{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module TestText ( tests ) where

import Distribution.TestSuite
import Distribution.TestSuite as TestSuite
import Replace.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Text as T
import Data.Bifunctor (second)

type Parser = Parsec Void T.Text

findAllCap' :: MonadParsec e s m => m a -> m [Either (Tokens s) (Tokens s, a)]
findAllCap' sep = sepCap (match sep)

findAll' :: MonadParsec e s f => f b -> f [Either (Tokens s) (Tokens s)]
findAll' sep = (fmap.fmap) (second fst) $ sepCap (match sep)

tests :: IO [Test]
tests = return
    [ Test $ runParserTest "findAll upperChar"
        (findAllCap' (upperChar :: Parser Char))
        ("aBcD" :: T.Text)
        [Left "a", Right ("B", 'B'), Left "c", Right ("D", 'D')]
    -- check that sepCap can progress even when parser consumes nothing
    -- and succeeds.
    , Test $ runParserTest "zero-consumption parser"
        (sepCap (many (upperChar :: Parser Char)))
        ("aBcD" :: T.Text)
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
        (findAll' ((takeWhileP Nothing (=='𝅘𝅥𝅯') :: Parser T.Text)))
        ("𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥" :: T.Text)
        [Left "𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥", Right "𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯", Left "𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥"]
    , Test $ runParserTest "empty input" (sepCap (fail "" :: Parser ())) "" []
    , Test $ streamEditTest "x to o"
        (string "x" :: Parser T.Text) (const "o")
        "x x x" "o o o"
    , Test $ streamEditTest "x to o inner"
        (string "x" :: Parser T.Text) (const "o")
        " x x x " " o o o "
    , Test $ streamEditTest "ordering"
        (string "456" :: Parser T.Text) (const "ABC")
        "123456789" "123ABC789"
    , Test $ streamEditTest "empty input" (match (fail "" :: Parser ())) (fst) "" ""
    , Test $ breakCapTest "basic" (upperChar :: Parser Char) "aAa" (Just ("a", 'A', "a"))
    , Test $ breakCapTest "first" (upperChar :: Parser Char) "Aa" (Just ("", 'A', "a"))
    , Test $ breakCapTest "last" (upperChar :: Parser Char) "aA" (Just ("a", 'A', ""))
    , Test $ breakCapTest "fail" (upperChar :: Parser Char) "aaa" Nothing
    , Test $ breakCapTest "match" (match (upperChar :: Parser Char)) "aAa" (Just ("a", ("A",'A'), "a"))
    , Test $ breakCapTest "zero-width" (lookAhead (upperChar :: Parser Char)) "aAa" (Just ("a", 'A', "Aa"))
    , Test $ breakCapTest "empty input" (upperChar :: Parser Char) "" Nothing
    , Test $ breakCapTest "empty input zero-width" (return () :: Parser ()) "" (Just ("", (), ""))
    -- I was unable to write a failing test for
    -- https://github.com/jamesdbrock/replace-megaparsec/issues/33
    -- but adding this "sep backtrack" test anyway
    , Test $ splitCapTest "sep backtrack" (match $ between (string "{{") (string "}}") (T.pack <$> many (alphaNumChar <|> spaceChar) :: Parser T.Text)) "{{foo.}}" [Left "{{foo.}}"]
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

    splitCapTest nam sep input expected = TestInstance
            { run = do
                let output = splitCap sep input
                if (output == expected)
                    then return (Finished Pass)
                    else return (Finished $ TestSuite.Fail
                                $ "got " <> show output <> " expected " <> show expected)
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


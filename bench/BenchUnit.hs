{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import Text.Megaparsec
import Replace.Megaparsec
import Criterion.Main
import Criterion.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as B.Builder
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Void

fooStringM :: String
fooStringM = take 1000000 $ cycle "       foo" -- a million bytes of foos
fooString10K :: String
fooString10K = take 10000 fooStringM
fooString100K :: String
fooString100K = take 100000 fooStringM

fooByteStringM :: B.ByteString
fooByteStringM = BL.toStrict fooByteStringLM
fooByteString10K :: B.ByteString
fooByteString10K = B.take 10000 fooByteStringM
fooByteString100K :: B.ByteString
fooByteString100K = B.take 100000 fooByteStringM

fooByteStringLM :: BL.ByteString
fooByteStringLM = B.Builder.toLazyByteString $ B.Builder.string8 fooStringM
fooByteStringL10K :: BL.ByteString
fooByteStringL10K = BL.take 10000 fooByteStringLM
fooByteStringL100K :: BL.ByteString
fooByteStringL100K = BL.take 100000 fooByteStringLM

fooTextM :: T.Text
fooTextM = T.pack fooStringM
fooText10K :: T.Text
fooText10K = T.take 10000 fooTextM
fooText100K :: T.Text
fooText100K = T.take 100000 fooTextM

fooTextLM :: TL.Text
fooTextLM = TL.pack fooStringM
fooTextL10K :: TL.Text
fooTextL10K = TL.take 10000 fooTextLM
fooTextL100K :: TL.Text
fooTextL100K = TL.take 100000 fooTextLM

main :: IO ()
main = defaultMainWith (defaultConfig
            { reportFile = Just "criterion-report.html"
            , resamples = 100
            })
    [ bgroup "String"
        [ bench "sepCap 10,000" $ whnf
            (parseMaybe (sepCap (chunk "foo" :: Parsec Void String String)))
            fooString10K
        , bench "streamEdit 10,000" $ whnf
            (streamEdit (chunk "foo" :: Parsec Void String String) (const "bar"))
            fooString10K
        , bench "sepCap 100,000" $ whnf
            (parseMaybe (sepCap (chunk "foo" :: Parsec Void String String)))
            fooString100K
        , bench "streamEdit 100,000" $ whnf
            (streamEdit (chunk "foo" :: Parsec Void String String) (const "bar"))
            fooString100K
        , bench "sepCap 1,000,000" $ whnf
            (parseMaybe (sepCap (chunk "foo" :: Parsec Void String String)))
            fooStringM
        , bench "streamEdit 1,000,000" $ whnf
            (streamEdit (chunk "foo" :: Parsec Void String String) (const "bar"))
            fooStringM
        ]
    , bgroup "ByteString.Strict"
        [ bench "sepCap 10,000" $ whnf
            (parseMaybe (sepCap (chunk "foo" :: Parsec Void B.ByteString B.ByteString)))
            fooByteString10K
        , bench "streamEdit 10,000" $ whnf
            (streamEdit (chunk "foo" :: Parsec Void B.ByteString B.ByteString) (const "bar"))
            fooByteString10K
        , bench "sepCap 100,000" $ whnf
            (parseMaybe (sepCap (chunk "foo" :: Parsec Void B.ByteString B.ByteString)))
            fooByteString100K
        , bench "streamEdit 100,000" $ whnf
            (streamEdit (chunk "foo" :: Parsec Void B.ByteString B.ByteString) (const "bar"))
            fooByteString100K
        , bench "sepCap 1,000,000" $ whnf
            (parseMaybe (sepCap (chunk "foo" :: Parsec Void B.ByteString B.ByteString)))
            fooByteStringM
        , bench "streamEdit 1,000,000" $ whnf
            (streamEdit (chunk "foo" :: Parsec Void B.ByteString B.ByteString) (const "bar"))
            fooByteStringM
        ]
    , bgroup "ByteString.Lazy"
        [ bench "sepCap 10,000" $ whnf
            (parseMaybe (sepCap (chunk "foo" :: Parsec Void BL.ByteString BL.ByteString)))
            fooByteStringL10K
        , bench "streamEdit 10,000" $ whnf
            (streamEdit (chunk "foo" :: Parsec Void BL.ByteString BL.ByteString) (const "bar"))
            fooByteStringL10K
        , bench "sepCap 100,000" $ whnf
            (parseMaybe (sepCap (chunk "foo" :: Parsec Void BL.ByteString BL.ByteString)))
            fooByteStringL100K
        , bench "streamEdit 100,000" $ whnf
            (streamEdit (chunk "foo" :: Parsec Void BL.ByteString BL.ByteString) (const "bar"))
            fooByteStringL100K
        , bench "sepCap 1,000,000" $ whnf
            (parseMaybe (sepCap (chunk "foo" :: Parsec Void BL.ByteString BL.ByteString)))
            fooByteStringLM
        , bench "streamEdit 1,000,000" $ whnf
            (streamEdit (chunk "foo" :: Parsec Void BL.ByteString BL.ByteString) (const "bar"))
            fooByteStringLM
        ]
    , bgroup "Text.Strict"
        [ bench "sepCap 10,000" $ whnf
            (parseMaybe (sepCap (chunk "foo" :: Parsec Void T.Text T.Text)))
            fooText10K
        , bench "streamEdit 10,000" $ whnf
            (streamEdit (chunk "foo" :: Parsec Void T.Text T.Text) (const "bar"))
            fooText10K
        , bench "sepCap 100,000" $ whnf
            (parseMaybe (sepCap (chunk "foo" :: Parsec Void T.Text T.Text)))
            fooText100K
        , bench "streamEdit 100,000" $ whnf
            (streamEdit (chunk "foo" :: Parsec Void T.Text T.Text) (const "bar"))
            fooText100K
        , bench "sepCap 1,000,000" $ whnf
            (parseMaybe (sepCap (chunk "foo" :: Parsec Void T.Text T.Text)))
            fooTextM
        , bench "streamEdit 1,000,000" $ whnf
            (streamEdit (chunk "foo" :: Parsec Void T.Text T.Text) (const "bar"))
            fooTextM
        ]
    , bgroup "Text.Lazy"
        [ bench "sepCap 10,000" $ whnf
            (parseMaybe (sepCap (chunk "foo" :: Parsec Void TL.Text TL.Text)))
            fooTextL10K
        , bench "streamEdit 10,000" $ whnf
            (streamEdit (chunk "foo" :: Parsec Void TL.Text TL.Text) (const "bar"))
            fooTextL10K
        , bench "sepCap 100,000" $ whnf
            (parseMaybe (sepCap (chunk "foo" :: Parsec Void TL.Text TL.Text)))
            fooTextL100K
        , bench "streamEdit 100,000" $ whnf
            (streamEdit (chunk "foo" :: Parsec Void TL.Text TL.Text) (const "bar"))
            fooTextL100K
        , bench "sepCap 1,000,000" $ whnf
            (parseMaybe (sepCap (chunk "foo" :: Parsec Void TL.Text TL.Text)))
            fooTextLM
        , bench "streamEdit 1,000,000" $ whnf
            (streamEdit (chunk "foo" :: Parsec Void TL.Text TL.Text) (const "bar"))
            fooTextLM
        ]
    ]


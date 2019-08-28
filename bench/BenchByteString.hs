{-# LANGUAGE OverloadedStrings #-}

import Text.Megaparsec
import Replace.Megaparsec
import Data.ByteString as BS

main :: IO ()
main = BS.getContents
        >>= streamEditT (chunk "foo") (return . const "bar")
        >>= BS.putStr

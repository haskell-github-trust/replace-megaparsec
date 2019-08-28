{-# LANGUAGE OverloadedStrings #-}

import Text.Megaparsec
import Replace.Megaparsec
import Data.Text.IO as T

main :: IO ()
main = T.getContents
    >>= streamEditT (chunk "foo") (return . const "bar")
    >>= T.putStr

import Text.Megaparsec
import Replace.Megaparsec

main :: IO ()
main = getContents
    >>= streamEditT (chunk "foo") (return . const "bar")
    >>= putStr

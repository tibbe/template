module Main where

import qualified Data.ByteString as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import Text.Template

-- | Create 'Context' from association list.
context :: [(String, String)] -> M.Map T.Text T.Text
context = M.fromList . map packPair
    where packPair (x, y) = (T.pack x, T.pack y)

main :: IO ()
main = S.putStr $ E.encodeUtf8 $ substitute helloTemplate helloContext
  where
    helloTemplate = T.pack "Hello, $name!\n"
    helloContext  = context [("name", "Joe")]

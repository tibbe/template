{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as S
import qualified Data.Map as M
import qualified Data.Text.Encoding as E

import Text.Template

main :: IO ()
main = S.putStr $ E.encodeUtf8 $ substitute helloTemplate helloContext
  where
    helloTemplate = "Hello, $name!\n"
    helloContext  = M.fromList [("name", "Joe")]

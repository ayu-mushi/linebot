{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import System.Environment

main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 8080 read $ lookup "PORT" env
  scotty port $ do
    get "/" $ html $ "Hello, Heroku!"

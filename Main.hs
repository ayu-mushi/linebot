{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import System.Environment
import Data.Monoid((<>))
import Network.HTTP.Types.Status()
import System.Random

--generateId :: IO [Int]
--generateId = do
--  g <- getStdGen
--  return $ map (\i -> i `mod` 10) $ take 8 ((fst $ random g) :: [Int])
--

main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 8080 read $ lookup "PORT" env
  scotty port $ do
    get "/" $ do
      html $ "Hello, Heroku!"
    get "/hello/:name" $ do
      name <- param "name"
      text $ "Hello, " <> name <> "!"
    get "/agent" $ do
      Just agent <- header "User-Agent"
      text agent
    get "/json" $ do
      json [(0::Int)..10]

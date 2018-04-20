{-# LANGUAGE OverloadedStrings,ScopedTypeVariables#-}

import Web.Scotty
import System.Environment
import Data.Monoid((<>))
import Network.HTTP.Types.Status()
import System.Random
import qualified Data.Text.Lazy as Text(pack, unpack)
import qualified Data.ByteString.Lazy as BS (pack, unpack, writeFile, readFile)
import Control.Monad.Trans(liftIO)
import Control.Exception (try, IOException)
import System.Directory (removeFile)


{-import Control.Lens ((^?))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Lens (key, _Array)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (for_)
import System.Environment (lookupEnv)

import Network.HTTP.Conduit
import Web.Spock.Safe-}


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
    get "/line" $ do
      lr <- liftIO $ try $ readFile "/tmp/linerequest.json"
      case lr of
        Right lr' -> text $ Text.pack lr'
        Left (e::IOException) -> text "File not found."
    post "/callback" $ do
      b <- body
      liftIO $ writeFile "/tmp/linerequest.json" $ show b

{-
{
  "replyToken": "nHuyWiB7yP5Zw52FIkcQobQuGDXCTA",
  "type": "message",
  "timestamp": 1462629479859,
  "source": {
    "type": "user",
    "userId": "U4af4980629..."
  },
  "message": {
    "id": "325708",
    "type": "text",
    "text": "Hello, world!"
  }
}-}

{-{"events":
  [{"type":"message",
  "replyToken":"fb5ff2b6d4764384be43105de58b6e4a",
  "source":{"userId":"Ub0292059288c2655f61486607cf0c7b9","type":"user"},
  "timestamp":1524182254956,
  "message":{"type":"text",
            "id":"7822858435458",
            "text":"Hey I am FAKE REAL"
            }
            }
            ]
            }-}

{-# LANGUAGE OverloadedStrings,ScopedTypeVariables#-}

import Web.Scotty as Scotty
import System.Environment
import Data.Monoid((<>))
import Network.HTTP.Types.Status()
import System.Random
import qualified Data.Text.Lazy as Text(pack, unpack, Text, toStrict, fromStrict)
import qualified Data.Text as StrictText(pack)
import Data.Text.IO as Text(writeFile, readFile)
import qualified Data.ByteString.Lazy as BS (pack, unpack, writeFile, readFile, fromStrict, toStrict)
import qualified Data.Text.Encoding  as Text(decodeUtf8)
import Control.Monad.Trans(liftIO)
import Control.Exception (try, IOException)
import Control.Monad.Catch as MonadCatch (catch, try, MonadCatch(..), Exception, MonadThrow, throwM)
import System.Directory (removeFile)
import qualified Data.ByteString.Lazy.UTF8 as BsUtf8 (foldl, toString, fromString)
import Web.Scotty.Trans(ScottyError(..))
import Web.Scotty.Internal.Types(ActionT(runAM, ActionT), ActionError)
import qualified Data.Attoparsec as AP (Result(..),parseOnly)
import Control.Lens
--import Text.JSON  as JSON(resultToEither, decode, showJSON, encode)
import Data.Aeson as Aeson
import Network.HTTP.Conduit
import qualified Codec.Binary.UTF8.String as Codec(encode, decode, encodeString, decodeString)

import Post as Post
import Get as Get

main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 8080 read $ lookup "PORT" env
  Just channelAccessToken <- lookupEnv "ACCESS_TOKEN"

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
      Scotty.json [(0::Int)..10]
    get "/line" $ do
      lr <- liftIO $ MonadCatch.try $ Text.readFile "/tmp/linerequest.json"
      case lr of
        Right lr' -> text $ Text.fromStrict lr'
        Left (e::IOException) -> text "File not found."
    post "/callback" $ do
      b <- body

      let lineev = fmap (head . Post.fromLINEReq) $ eitherDecode b :: Either String Post.LINEEvent
      let message = fmap (^. Post.evMessage . Post.msText) lineev
      let (Right rep_tok) = fmap (^. Post.evReplyToken) lineev

      case message of
        Left _ -> liftIO $ Text.writeFile "/tmp/linerequest.json" "NANTOKA Error."
        Right yes -> do
          lineReply channelAccessToken rep_tok yes


lineReply :: (ScottyError e) => String -> String -> String -> ActionT e IO ()
lineReply channelAccessToken rep_tok message = do
  request <- parseUrl "https://api.line.me/v2/bot/message/reply"
  let postRequest = request {
    method = "POST"
    , requestHeaders =
      [ ("Content-Type", "application/json; charser=UTF-8")
        , ("Authorization", BS.toStrict $ BsUtf8.fromString $ "Bearer " ++ channelAccessToken)
        ]
    , requestBody = RequestBodyLBS $ encode $ defReplyText rep_tok message
    }
  manager <- liftIO $ newManager tlsManagerSettings
  httpLbs postRequest manager
  return ()

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
instance (MonadThrow m, ScottyError e) => MonadThrow (ActionT e m) where
  throwM = ActionT . throwM

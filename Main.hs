{-# LANGUAGE OverloadedStrings,ScopedTypeVariables#-}

import Web.Scotty
import System.Environment
import Data.Monoid((<>))
import Network.HTTP.Types.Status()
import System.Random
import qualified Data.Text.Lazy as Text(pack, unpack, Text, toStrict, fromStrict)
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
import Text.JSON  as JSON(resultToEither, decode, showJSON, encode)
import Network.HTTP.Conduit

import Post as Post
import Get as Get
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
  Just channelAccessToken    <- lookupEnv "ACCESS_TOKEN"

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
      lr <- liftIO $ MonadCatch.try $ Text.readFile "/tmp/linerequest.json"
      case lr of
        Right lr' -> text $ Text.fromStrict lr'
        Left (e::IOException) -> text "File not found."
    post "/callback" $ do
      b <- body
      let message = fmap ((^. Post.evMessage . Post.msText) . head . Post.fromLINEReq) (resultToEither $ decode $ BsUtf8.toString b :: Either String Post.LINEReq)
      let (Right rep_tok) = fmap ((^. Post.evReplyToken) . head . Post.fromLINEReq) (resultToEither $ decode $ BsUtf8.toString b :: Either String Post.LINEReq)
      case message of
        Left _ -> liftIO $ Text.writeFile "/tmp/linerequest.json" "NANTOKA Error."
        Right yes -> do
          liftIO $ Text.writeFile "/tmp/linerequest.json" $ Text.toStrict $ Text.pack $ "length:" ++ show (length yes) ++ "," ++"first character is: " ++[(head yes)]++","++ yes
          request <- parseUrl "https://api.line.me/v2/bot/message/reply"
          let postRequest = request {
            method = "POST"
            , requestHeaders = [ ("Content-Type", "application/json; charser=UTF-8")
                         , ("Authorization", BS.toStrict $ BsUtf8.fromString $ "Bearer " ++ channelAccessToken)
                         ]
            , requestBody = RequestBodyLBS $ BsUtf8.fromString $ encode $ defReplyText rep_tok "Hello! こんにちは!"
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

instance (MonadCatch m, ScottyError e) => MonadCatch (ActionT e m) where
  catch (ActionT m) f = ActionT $ m `MonadCatch.catch` (runAM . f)

instance ScottyError IOException where
  stringError = userError
  showError = Text.pack . show

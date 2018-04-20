{-# LANGUAGE OverloadedStrings,ScopedTypeVariables#-}

import Web.Scotty
import Data.Map as Map (fromList, (!))
import System.Environment
import Data.Monoid((<>))
import Network.HTTP.Types.Status()
import System.Random
import qualified Data.Text.Lazy as Text(pack, unpack, Text)
import Data.Text.IO as Text(writeFile)
import qualified Data.ByteString.Lazy as BS (pack, unpack, writeFile, readFile)
import qualified Data.Text.Encoding  as Text(decodeUtf8)
import Control.Monad.Trans(liftIO)
import Control.Exception (try, IOException)
import Control.Monad.Catch as MonadCatch (catch, try, MonadCatch(..), Exception, MonadThrow, throwM)
import System.Directory (removeFile)
import qualified Data.ByteString.Lazy.UTF8 as BsUtf8 (foldl, toString)
import Text.JSON
import Control.Monad (mplus, mzero)
import Web.Scotty.Trans(ScottyError(..))
import Web.Scotty.Internal.Types(ActionT(runAM, ActionT), ActionError)
import qualified Data.Attoparsec as AP (Result(..),parseOnly)

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
      lr <- liftIO $ MonadCatch.try $ readFile "/tmp/linerequest.json"
      case lr of
        Right lr' -> text $ Text.pack lr'
        Left (e::IOException) -> text "File not found."
    post "/callback" $ do
      b <- body
      liftIO $ Prelude.writeFile "/tmp/linerequest.json" $ show $ (resultToEither $ decode $ BsUtf8.toString b :: Either String LINEReq)

newtype LINEReq = Events { fromLINEReq :: [LINEEvent] } deriving Show

data LINEEvent = LINEEvent {
  evType :: String
  , evReplyToken :: String
  , evTimeStamp:: Int
  , evMessage :: Message
  } deriving Show

data Message = Message {
  msType :: String
  , msId :: Int
  , msText :: String
  } deriving Show

instance JSON LINEReq where
  readJSON (JSObject obj) = do
    let map = Map.fromList $ fromJSObject obj
    JSArray (arr::[JSValue]) <- readJSON $ map ! "events"
    (evs::[LINEEvent]) <- mapM readJSON arr
    return $ Events evs
  readJSON _ = mzero
  showJSON (Events evs) = makeObj [ ("events", showJSON evs) ]

instance JSON Message where
  readJSON (JSObject obj) = do
    let map = Map.fromList $ fromJSObject obj
    mtype    <- readJSON$map!"type"
    mid <- readJSON$ map!"id"
    text <- readJSON $ map ! "text"
    return $ Message mtype (read mid::Int) text

  readJSON _ = mzero
  showJSON (Message typ msid text) =
    makeObj [ ("type", showJSON typ)
           , ("id", showJSON msid)
           , ("text", showJSON text)
           ]

instance JSON LINEEvent where
  readJSON (JSObject obj) = do
    let map = Map.fromList $ fromJSObject obj
    typ <- readJSON $ map ! "type"
    tok <- readJSON $ map ! "replyToken"
    time <- readJSON $ map ! "timestamp"
    mess <- readJSON $ map ! "message"
    return $ LINEEvent typ  tok  time  mess

  readJSON _ = mzero

  showJSON (LINEEvent typ tok stamp mess) =
    makeObj [ ("type", showJSON typ)
           , ("replyToken", showJSON tok)
           , ("timestamp", showJSON stamp)
           , ("message", showJSON mess)
           ]

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

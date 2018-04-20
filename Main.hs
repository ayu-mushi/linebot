{-# LANGUAGE OverloadedStrings,ScopedTypeVariables#-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Char (isDigit)
import Web.Scotty
import Data.Map as Map (fromList, (!))
import System.Environment
import Data.Monoid((<>))
import Network.HTTP.Types.Status()
import System.Random
import qualified Data.Text.Lazy as Text(pack, unpack, Text, toStrict, fromStrict)
import Data.Text.IO as Text(writeFile, readFile)
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
import Text.Parsec
import Control.Lens

data Message = Message {
  _msType :: String
  , _msId :: Int
  , _msText :: String
  } deriving Show

makeLenses ''Message

data LINEEvent = LINEEvent {
  _evType :: String
  , _evReplyToken :: String
  , _evTimeStamp:: Int
  , _evMessage :: Message
  } deriving Show

makeLenses ''LINEEvent

newtype LINEReq = Events { fromLINEReq :: [LINEEvent] } deriving Show


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
      lr <- liftIO $ MonadCatch.try $ Text.readFile "/tmp/linerequest.json"
      case lr of
        Right lr' -> text $ Text.fromStrict lr'
        Left (e::IOException) -> text "File not found."
    post "/callback" $ do
      b <- body
      let parsed = fmap decodeUtf8FromStrToStr' $ fmap ((^. evMessage . msText).head.fromLINEReq) (resultToEither $ decode $ BsUtf8.toString b :: Either String LINEReq)
      case parsed of
        Left _ -> liftIO $ Text.writeFile "/tmp/linerequest.json" "Parse Error."
        Right cv -> case cv of
                         Left _ -> liftIO $ Text.writeFile "/tmp/linerequest.json" "NANTOKA Error."
                         Right yes -> liftIO $ Text.writeFile "/tmp/linerequest.json" $ Text.toStrict $ Text.pack $ "length:" ++ show (length yes) ++ "," ++ yes



instance JSON LINEReq where
  readJSON (JSObject obj) = do
    let mobj = Map.fromList $ fromJSObject obj
    JSArray (arr::[JSValue]) <- readJSON $ mobj ! "events"
    (evs::[LINEEvent]) <- mapM readJSON arr
    return $ Events evs
  readJSON _ = mzero
  showJSON (Events evs) = makeObj [ ("events", showJSON evs) ]

instance JSON Message where
  readJSON (JSObject obj) = do
    let mobj = Map.fromList $ fromJSObject obj
    mtype    <- readJSON$ mobj!"type"
    mid <- readJSON$ mobj!"id"
    text <- readJSON $ mobj ! "text"
    let (Right parsed) = decodeUtf8FromStrToStr' text
    return $ Message mtype (read mid::Int) parsed

  readJSON _ = mzero
  showJSON (Message typ msid text) =
    makeObj [ ("type", showJSON typ)
           , ("id", showJSON msid)
           , ("text", showJSON text)
           ]

instance JSON LINEEvent where
  readJSON (JSObject obj) = do
    let mobj = Map.fromList $ fromJSObject obj
    typ <- readJSON $ mobj ! "type"
    tok <- readJSON $ mobj ! "replyToken"
    time <- readJSON $ mobj ! "timestamp"
    mess <- readJSON $ mobj ! "message"
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
literalUtf8 :: Parsec String u Char
literalUtf8 = do
  slash <- char '\\'
  n <- many digit
  return $ (toEnum (read n :: Int) :: Char)

literalBSlash :: Parsec String u Char
literalBSlash = do
  slash <- char '\\'
  slash <- char '\\'
  return '\\'

literalNewLineChar :: Parsec String u Char
literalNewLineChar = do
  slash <- char '\\'
  slash <- char 'n'
  return '\n'

decodeUtf8FromStrToStr :: Parsec String u String
decodeUtf8FromStrToStr = many $ literalUtf8 `mplus` literalBSlash `mplus` anyChar

decodeUtf8FromStrToStr' :: String -> Either ParseError String
decodeUtf8FromStrToStr' = parse decodeUtf8FromStrToStr ""


instance (MonadThrow m, ScottyError e) => MonadThrow (ActionT e m) where
    throwM = ActionT . throwM

instance (MonadCatch m, ScottyError e) => MonadCatch (ActionT e m) where
  catch (ActionT m) f = ActionT $ m `MonadCatch.catch` (runAM . f)

instance ScottyError IOException where
  stringError = userError
  showError = Text.pack . show

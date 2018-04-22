{-# LANGUAGE OverloadedStrings,ScopedTypeVariables, LambdaCase #-}

import Web.Scotty as Scotty
import System.Environment
import Data.Monoid((<>))
import Network.HTTP.Types.Status()
import System.Random
import qualified Data.Text.Lazy as Text(pack, unpack, Text, toStrict, fromStrict)
import qualified Data.Text as StrictText(pack)
import Data.Text.IO as Text(writeFile, readFile)
import qualified Data.ByteString.Lazy as BS (pack, unpack, writeFile, readFile, fromStrict, toStrict, ByteString)
import qualified Data.ByteString as BSStrict (ByteString, pack, unpack)
import qualified Data.Text.Encoding  as Text(decodeUtf8)
import Control.Monad.Trans(liftIO, lift, MonadIO)
import Control.Exception (try, IOException)
import Control.Monad (mplus, mzero, MonadPlus, msum)
import Control.Monad.Catch as MonadCatch (catch, try, MonadCatch(..), Exception, MonadThrow, throwM)
import qualified Data.ByteString.Lazy.UTF8 as BsUtf8 (foldl, toString, fromString)
import Web.Scotty.Trans(ScottyError(..))
import Web.Scotty.Internal.Types(ActionT(runAM, ActionT), ActionError)
import Control.Lens
--import Text.JSON  as JSON(resultToEither, decode, showJSON, encode)
import Data.Aeson as Aeson
import Network.HTTP.Conduit
import qualified Codec.Binary.UTF8.String as Codec(encode, decode, encodeString, decodeString)
import Control.Concurrent (threadDelay)
import Text.Parsec.Error as Parsec(Message(UnExpect, Expect,SysUnExpect, Message), errorMessages)

import Post as Post
import Get as Get
import Text.Parsec as Parsec

main :: IO ()
main = do
  env <- getEnvironment
  let port = maybe 8080 read $ lookup "PORT" env
  channelAccessToken <- accessToken

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
      lr <- liftIO $ MonadCatch.try $ Text.readFile "/tmp/linerequest.txt"
      case lr of
        Right lr' -> text $ Text.fromStrict lr'
        Left (e::IOException) -> text "File not found."
    post "/callback" $ do
      b <- body

      let lineev = fmap (head . Post.fromLINEReq) $ eitherDecode b :: Either String Post.LINEEvent
      let (Right message) = fmap (^. Post.evMessage . Post.msText) lineev
      let (Right user_id) = fmap (^. Post.evSource . Post.srcUserId) lineev
      let (Right rep_tok) = fmap ((^. Post.evReplyToken)) lineev

      strMay <- runParserT mainParser "" "" message
      lineReply channelAccessToken rep_tok $ either ifError id strMay
      return ()

ifError :: ParseError -> String
ifError err = concat $ flip map (errorMessages err) $ \case
  SysUnExpect x -> "sys unexpected: " ++ show x
  Expect x -> "expected: " ++ show x
  UnExpect x -> "unexpected: " ++ show x
  Parsec.Message x -> "message" ++ show x

mainParser  :: (MonadIO m) => ParsecT String u m String
mainParser = do
  star <- msum $ map char "☆λ$@%:"
  str <- helpParser <|> secondParser <|> parrotParser
  return str

mappMaybe :: MonadPlus m => Maybe a -> (a -> m b) -> m b
mappMaybe may mapp =
  case may of
    Just a -> mapp a
    Nothing -> mzero

secondParser :: (MonadIO m) => ParsecT String u m String
secondParser = Parsec.try $ do
  numeric <- Parsec.many Parsec.digit
  Parsec.string "秒後"
  Parsec.eof
  lift $ liftIO $ threadDelay $ read numeric * (10^6)
  return $ numeric ++ "秒経過しました！！！！"

parrotParser ::  (Monad m) => ParsecT String u m String
parrotParser = many anyToken

-- LINE Script
-- 機能候補: 名前からメッセージを送る機能

lsParser ::  (Monad m) => ParsecT String u m String
lsParser = Parsec.try $ do
  _ <- string "ls"
  return ""

data LSSentense a = DefVar a | InitVar a (LSFormula a) | Seq (LSSentense a) (LSSentense a) | Substitution a (LSFormula a)
data LSFormula a = UseVar a | LSTrue | LSFalse | LSNumberC Float
data LSType = LSBool | LSString | LSNumber | LSArray

newtype AccessToken = AccessToken { unAccessToken :: String } deriving (Eq)

accessToken :: IO AccessToken
accessToken = do
  Just channelAccessToken <- lookupEnv "ACCESS_TOKEN"
  return $ AccessToken channelAccessToken

bearer :: AccessToken -> BSStrict.ByteString
bearer channelAccessToken = BS.toStrict $ BsUtf8.fromString $ "Bearer " ++ unAccessToken channelAccessToken

lineReply :: (ScottyError e) => AccessToken -> ReplyToken -> String -> ActionT e IO (Response BS.ByteString)
lineReply channelAccessToken rep_tok message = do
  req <- parseUrl "https://api.line.me/v2/bot/message/reply"
  let postRequest = req {
    method = "POST"
    , requestHeaders =
      [ ("Content-Type", "application/json; charser=UTF-8")
        , ("Authorization", bearer channelAccessToken)
        ]
    , requestBody = RequestBodyLBS $ encode $ defReplyText rep_tok message
    }
  manager <- liftIO $ newManager tlsManagerSettings
  httpLbs postRequest manager

linePush :: (ScottyError e) => AccessToken -> UserId -> String -> ActionT e IO (Response BS.ByteString)
linePush channelAccessToken uid message = do
  req <- parseUrl "https://api.line.me/v2/bot/message/push"
  let postRequest = req {
    method = "POST"
    , requestHeaders =
      [ ("Content-Type", "application/json; charser=UTF-8")
        , ("Authorization", bearer channelAccessToken)
        ]
    , requestBody = RequestBodyLBS $ encode $ defPushText uid message
    }
  manager <- liftIO $ newManager tlsManagerSettings
  httpLbs postRequest manager

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

appName :: String
appName = "かわさきが作った人工無脳(仮) var 0.1."

-- なつくようにする

helpParser :: Monad m => ParsecT String u m String
helpParser = Parsec.try $ do
  _ <- string "help"
  Parsec.eof

  return $ appName <> "\n\
  \help: ($|☆|λ|@|%|:)で始まるメッセージを認識します。\n \
  \その以後が以下のようなパターンのときに処理を行います。\n \
  \「help」: このヘルプを表示する。\n \
  \「([1-9]*)秒後」:  数字の部分の秒数待った後で通知します。\n \
  \その他: オウム返しします。\
  \"

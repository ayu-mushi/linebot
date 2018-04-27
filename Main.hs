{-# LANGUAGE OverloadedStrings,ScopedTypeVariables, LambdaCase, DoAndIfThenElse, BangPatterns#-}

import Web.Scotty as Scotty
import Data.Maybe (fromMaybe)
import Data.Monoid(First(..), (<>))
import System.Environment
import System.Directory (removeFile, doesFileExist)
import System.IO(withFile, openFile, IOMode(ReadWriteMode), hPutStr, hPutStrLn, hClose, hGetLine)
import System.IO.Strict as Strict(readFile)
import Network.HTTP.Types.Status()
import System.Random
import qualified Data.Text.Lazy as Text(pack, unpack, Text, toStrict, fromStrict)
import qualified Data.Text as StrictText(pack)
import Data.Text.IO as Text(writeFile, readFile)
import qualified Data.ByteString.Lazy as BS (pack, unpack, writeFile, readFile, fromStrict, toStrict, ByteString)
import qualified Data.ByteString as BSStrict (ByteString, pack, unpack)
import qualified Data.Text.Encoding  as Text(decodeUtf8)
import Control.Monad.Trans(liftIO, lift, MonadIO)
import Control.Exception (try, IOException, SomeException, throwIO)
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
import Control.Monad.State(execStateT, runStateT, evalStateT)
import Data.List(nub)

import Post as Post
import Get as Get
import Text.Parsec as Parsec
import qualified Shogi as Shogi

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
      Scotty.json [(0::Int)..10]
    get "/line" $ do
      lr <- liftIO $ MonadCatch.try $ Prelude.readFile "/tmp/linerequest.txt"
      case lr of
        Right lr' -> html $ Text.pack lr'
        Left (e::IOException) -> html "File not found."
    get "/shogi/:move" $ do
      mv <- param "move"
      Right parsed <- runParserT shogiParser "" "" $ "shogi " <> mv
      text $ Text.pack $ {-Shogi.shogiTest -}parsed
    post "/callback" $ do
      channelAccessToken <- lift accessToken
      b <- body

      let lineev = fmap (head . Post.fromLINEReq) $ eitherDecode b :: Either String Post.LINEEvent

      let (Right user_id) = fmap (^. Post.evSource . Post.srcUserId) lineev
      let (Right group_id) = fmap ((^. Post.evSource . Post.srcGroupId)) lineev
      let (First (Just line_id)) = (First $ (Left <$> group_id) :: (First (Either GroupId UserId))) <> (First $ (Right <$> user_id))
      let (Right rep_tok) = fmap ((^. Post.evReplyToken)) lineev

      let (Right typ) = fmap (^. Post.evType) lineev

      if (typ == "join") then do
        strMay <- runParserT (mainParser line_id) "" "" "☆help"
        linePush channelAccessToken line_id $ either ifError id strMay
        return ()
      else if (typ == "follow") then do
        strMay <- runParserT (mainParser line_id) "" "" "☆help"
        linePush channelAccessToken line_id $ either ifError id strMay
        return ()
      else if (typ == "message") then do
        let (Right (Just message)) = fmap (^. Post.evMessage) lineev
        let text = message ^. Post.msText
        strMay <- runParserT (mainParser line_id) "" "" text
        linePush channelAccessToken line_id $ either ifError id strMay
        return ()
      else return ()

      return ()

mapParseError :: (String -> String) -> Parsec.Message -> Parsec.Message
mapParseError f err =
  case err of
    SysUnExpect x -> SysUnExpect $ f x
    Expect x -> Expect $ f x
    UnExpect x -> Expect $ f x
    Parsec.Message x -> Parsec.Message $ f x


ifError :: ParseError -> String
ifError err = concat $ flip map (filter (\case SysUnExpect x-> False; _ -> True) $ errorMessages err) $ \case
  Expect x | either (const False) (const True) $ parse errorParser "" x -> case either (const "parseError.") id $ parse errorParser "" x of
                   [x] -> if x `elem` thisappchar then "" else "expected: " ++ [x] ++ ".\n"
                   xs -> "expected: " ++ xs ++ ".\n"
           | otherwise -> "expected: " ++ x ++ ".\n"
  UnExpect x -> "unexpected: " ++ x ++ ".\n"
  Parsec.Message "sleeping" -> ""
  Parsec.Message x -> "message" ++ x ++ ".\n"

thisappchar = "☆λ$@%:"

unicodeParser :: Parsec String u Char
unicodeParser = Parsec.try $ do
  slash <- char '\\'
  numeric <- many1 digit
  return (toEnum (read numeric::Int) :: Char)

errorParser :: Parsec String u String
errorParser = do
  q <- char '\"'
  xxx <- many1 $ unicodeParser <|> satisfy (/='\"')
  q2 <- char '\"'
  eof
  return xxx

mainParser  :: (MonadIO m) => Either GroupId UserId -> ParsecT String u m String
mainParser id_either = do
  str <- lift $ liftIO $ Control.Exception.try $ Prelude.readFile "is_sleep.txt"
  case str of
       Left (a::IOException) -> return ()
       Right str -> if read str == id_either then fail "sleeping" else return ()
  star <- msum $ map char thisappchar
  str <- helpParser <|> secondParser <|> sleepParser id_either <|> parrotParser <|> shogiParser
  return str

mappMaybe :: MonadPlus m => Maybe a -> (a -> m b) -> m b
mappMaybe may mapp =
  case may of
    Just a -> mapp a
    Nothing -> mzero

secondParser :: (MonadIO m) => ParsecT String u m String
secondParser = Parsec.try $ do
  numeric <- Parsec.many1 Parsec.digit <?> "\"list of digit\""
  Parsec.string "秒後"
  Parsec.eof
  lift $ liftIO $ threadDelay $ read numeric * (10^6)
  return $ numeric ++ "秒経過しました！！！！"

-- try の位置を変える

parrotParser ::  (Monad m) => ParsecT String u m String
parrotParser = do
  _ <- msum $ map string ["オウム", "parrot", "鏡", "mirror", "エコー", "echo"]
  many anyToken


sleepParser ::  (MonadIO m) => Either GroupId UserId -> ParsecT String u m String
sleepParser id_either = Parsec.try $ do
  _ <- msum $ map string ["sleep", "眠れ", "眠る"]
  skipMany space
  numeric <- many1 digit <|> return "10"
  lift $ liftIO $ Prelude.writeFile "is_sleep.txt" $ show id_either
  lift $ liftIO $ threadDelay $ (read numeric) * (10^6)
  lift $ liftIO $ removeFile "is_sleep.txt"
  return ""

-- LINE Script
-- 機能候補: 名前からメッセージを送る機能



shogiParser :: (MonadIO m) => ParsecT String u m String
shogiParser = do
  _ <- string "shogi" <|> string "将棋"
  skipMany space

  str <- (do
    mv <- Shogi.moveParser
    !old_field_str <- lift $ liftIO $ Strict.readFile "shogi.txt" `catch` (\(e::IOException) -> return $ show [Shogi.initialField])
    let old_field = read old_field_str :: [Shogi.Field]
    let newField = nub $ concatMap (Shogi.move mv) old_field

    lift $ liftIO $ Prelude.writeFile "shogi.txt" $ show (newField :: [Shogi.Field])
    return $ concat $ map ((++"\n").Shogi.showField) newField
    ) <|> (do
      _ <- string "init"
      lift $ liftIO $ Prelude.writeFile "shogi.txt" $ show [Shogi.initialField]
      return $ Shogi.showField Shogi.initialField
      ) <|> (do
      _ <- string "display"
      skipMany space
      !(old_field::[Shogi.Field]) <-  (do
        _ <- string "reverse"
        fmap (map Shogi.reverseField) $ fmap read $ lift $ liftIO $ Strict.readFile "shogi.txt" `catch` (\(e::IOException) -> return $ show [Shogi.initialField])) <|> (fmap read $ lift $ liftIO $ Strict.readFile "shogi.txt" `catch` (\(e::IOException) -> return $ show [Shogi.initialField]))

      return $ concatMap ((++"\n").Shogi.showField) $ (old_field :: [Shogi.Field])
      )
  return str

lsParser ::  (Monad m) => ParsecT String u m String
lsParser = Parsec.try $ do
  _ <- string "ls"
  return ""

data LSSentense a = DefVar a (LSSentense a) | InitVar a (LSFormula a) (LSSentense a) | Seq (LSSentense a) (LSSentense a) | Substitution a (LSFormula a)
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

linePush :: (ScottyError e) => AccessToken -> Either GroupId UserId -> String -> ActionT e IO (Response BS.ByteString)
linePush channelAccessToken uid message = do
  req <- parseUrl "https://api.line.me/v2/bot/message/push"
  let postRequest = req {
    method = "POST"
    , requestHeaders =
      [ ("Content-Type", "application/json; charser=UTF-8")
        , ("Authorization", bearer channelAccessToken)
        ]
    , requestBody = RequestBodyLBS $ encode $ defPushTextEither uid message
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
appName = "人工無脳(仮) ver 0.1."

-- なつくようにする

helpParser :: Monad m => ParsecT String u m String
helpParser = Parsec.try $ do
  _ <- string "help"
  Parsec.eof

  return $ appName <> "\
  \ \nhelp: [☆$λ@%:]で始まるメッセージを認識します。\
  \ \nその以後が以下のようなパターンのときに処理を行います。\
  \ \n「help」: このヘルプを表示する。\
  \ \n「([0-9]+)秒後」:  数字の部分を自然数として解釈し、その秒数待った後で通知します。ex. '@10秒後'\
  \ \n「sleep [0-9]+」:  数字の部分を自然数として解釈し、その秒数(デフォルト: 10)の間死にます。ex. '@sleep 10'\
  \ \n「(オウム)|(parrot)|鏡|(mirror)|(エコー)|(echo)」: オウム返しします。ex. '@parrot AAA'\
  \ \n「(shogi)|(将棋) init」: 将棋を最初からします。ex. '@shogi init'\
  \ \n「(shogi)|(将棋) display」: 将棋の現状態を表示します。オプション: reverseをつけると反転して表示します。ex. '@shogi display reverse'\
  \ \n「(shogi)|(将棋) [1-9][一-九][歩銀王..]」: 駒を動かします。ex. '@shogi 58金右'\
  \"

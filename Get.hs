{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings,ScopedTypeVariables#-}

module Get
( Message(Message)
, msType
, msText
, Reply(Reply)
, Push(Push)
, repToken
, repMess
, defReply
, defReplyText
, defPushText
, ReplyToken(..)
, UserId(..)
) where

import Data.Map as Map (fromList, (!))
import Control.Lens hiding ((.=))
import Text.JSON as JSON
import Data.Aeson as Aeson
import Control.Monad (mplus, mzero)
import qualified Data.ByteString.Lazy as BS (ByteString)

data Message = Message {
  _msType :: String
  , _msText :: String
  } deriving (Show)

makeLenses ''Message

newtype ReplyToken = ReplyToken { unReplyToken :: String } deriving (Show, Eq)

data Reply = Reply{
  _repToken :: ReplyToken
  ,_repMess :: [Message]
  } deriving (Show)

makeLenses ''Reply

newtype UserId = UserId { unUserId :: String } deriving (Show, Eq)

data Push = Push{
  _pushTo :: UserId
  ,_pushMess :: [Message]
  }
makeLenses ''Push

instance JSON Message where
  readJSON (JSObject obj) = do
    let mobj = Map.fromList $ fromJSObject obj
    typ <- readJSON $ mobj ! "type"
    text <- readJSON $ mobj ! "text"
    return $ Message typ text
  readJSON _ = mzero
  showJSON ms = makeObj [ ("type", showJSON $ ms ^. msType),  ("text", showJSON $ ms ^. msText)]

instance JSON Reply where
  readJSON (JSObject obj) = do
    let mobj = Map.fromList $ fromJSObject obj
    tok    <- readJSON$ mobj!"replyToken"
    JSArray mess <- readJSON$ mobj!"messages"
    (mess'::[Message]) <- mapM readJSON mess
    return $ Reply (ReplyToken tok) mess'

  readJSON _ = mzero
  showJSON (Reply tok mess) =
    makeObj [ ("replyToken", showJSON $ unReplyToken tok),
              ("messages", JSArray $ map showJSON mess)
           ]

instance FromJSON Message where
  parseJSON (Object v) = do
    t <- v .: "type"
    text <- v .: "text"
    return $ Message t text

instance FromJSON Reply where
  parseJSON (Object v) = do
    rt <- v .: "replyToken"
    ms <- v .: "messages"
    return $ Reply (ReplyToken rt) ms

instance ToJSON Message where
  toJSON v = object
    ["type" .= (v ^. msType),
     "text" .= (v ^. msText)]

instance ToJSON Reply where
  toJSON v = object ["replyToken" .= (unReplyToken $ v ^. repToken),
                     "messages" .= (v ^. repMess)]

instance ToJSON Push where
  toJSON v = object ["to" .= (unUserId $ v ^. pushTo),
                     "messages" .= (v ^. pushMess)]
defMessage :: Message
defMessage = Message "text" ""

defReply :: ReplyToken -> Reply
defReply token = Reply token [defMessage]

defReplyText :: ReplyToken -> String -> Reply
defReplyText token text = Reply token [Message "text" text]

defPushText :: UserId -> String -> Push
defPushText to text = Push {_pushTo = to, _pushMess=[Message "text" text]}

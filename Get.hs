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
, defPushTextG
, defPushTextEither
, ReplyToken(..)
, UserId(..)
, GroupId(..)
) where

import Control.Lens hiding ((.=))
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

newtype UserId = UserId { unUserId :: String } deriving (Show, Eq, Read)
newtype GroupId = GroupId { unGroupId :: String } deriving (Show, Eq, Read)

data Push = Push{
  _pushTo :: Either GroupId UserId
  ,_pushMess :: [Message]
  }
makeLenses ''Push

instance FromJSON Message where
  parseJSON (Object v) = do
    t <- v .: "type"
    text <- v .: "text"
    return $ Message t text
  parseJSON _ = mzero

instance FromJSON Reply where
  parseJSON (Object v) = do
    rt <- v .: "replyToken"
    ms <- v .: "messages"
    return $ Reply (ReplyToken rt) ms
  parseJSON _ = mzero

instance ToJSON Message where
  toJSON v = object
    ["type" .= (v ^. msType),
     "text" .= (v ^. msText)]

instance ToJSON Reply where
  toJSON v = object ["replyToken" .= (unReplyToken $ v ^. repToken),
                     "messages" .= (v ^. repMess)]

instance ToJSON Push where
  toJSON v = object ["to" .= (either unGroupId unUserId $ v ^. pushTo),
                     "messages" .= (v ^. pushMess)]

defMessage :: Message
defMessage = Message "text" ""

defReply :: ReplyToken -> Reply
defReply token = Reply token [defMessage]

defReplyText :: ReplyToken -> String -> Reply
defReplyText token text = Reply token [Message "text" text]

defPushText :: UserId -> String -> Push
defPushText to text = Push {_pushTo = Right to, _pushMess=[Message "text" text]}

defPushTextG :: GroupId -> String -> Push
defPushTextG to text = Push {_pushTo = Left to, _pushMess=[Message "text" text]}

defPushTextEither :: Either GroupId UserId -> String -> Push
defPushTextEither to text = Push {_pushTo = to, _pushMess=[Message "text" text]}

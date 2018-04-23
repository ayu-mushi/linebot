{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings,ScopedTypeVariables#-}

module Post (
  Message(Message), msType, msId, msText, LINEEvent(LINEEvent), evType, evReplyToken, evTimeStamp, evMessage, LINEReq(..),
  evSource, srcUserId, srcGroupId,srcType) where

import Control.Lens
import Control.Monad (mzero)
import Data.Aeson as Aeson
import Get (UserId(..), ReplyToken(..), GroupId(..))

data Source = Source {
  _srcUserId :: Maybe UserId
  ,_srcType :: String
  ,_srcGroupId :: Maybe GroupId
  } deriving (Show)

makeLenses ''Source

data Message = Message {
  _msType :: String
  , _msId :: Int
  , _msText :: String
  } deriving Show

makeLenses ''Message

data LINEEvent = LINEEvent {
  _evType :: String
  , _evReplyToken :: ReplyToken
  , _evSource :: Source --"source":{"userId":"Ub0292059288c2655f61486607cf0c7b9","type":"user"},
  , _evTimeStamp:: Int
  , _evMessage :: Maybe Message
  } deriving Show

makeLenses ''LINEEvent



newtype LINEReq = Events { fromLINEReq :: [LINEEvent] } deriving Show

instance FromJSON LINEReq where
  parseJSON (Object v) = do
    (arr::[LINEEvent]) <- v .: "events"
    return $ Events arr
  parseJSON _ = mzero

instance FromJSON Message where
  parseJSON (Object v) = do
    t <- v .: "type"
    i <- v .: "id"
    text <- v .: "text"
    return $ Message t (read i::Int) text
  parseJSON _ = mzero

instance FromJSON LINEEvent where
  parseJSON (Object v) = do
    t <- v .: "type"
    rt <- v .: "replyToken"
    src <- v .: "source"
    ts <- v .: "timestamp"
    (ms::Maybe Message) <- v .:? "message"
    return $ LINEEvent { _evType =t, _evReplyToken=(ReplyToken rt), _evSource=src, _evTimeStamp=ts, _evMessage=ms}

  parseJSON _ = mzero

instance FromJSON Source where
  parseJSON (Object v) = do
    typ <- v .: "type"
    uid <- v .:? "userId"
    gid <- v .:? "groupId"
    return $ Source { _srcUserId = fmap UserId uid, _srcType =typ, _srcGroupId = fmap GroupId gid}

  parseJSON _ = mzero

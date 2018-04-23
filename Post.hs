{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings,ScopedTypeVariables#-}

module Post (
  Message(Message), msType, msId, msText, LINEEvent(LINEEvent), evType, evReplyToken, evTimeStamp, evMessage, LINEReq(..),
  evSource, srcUserId, srcGroupId,srcType) where

import Control.Lens
import Text.JSON as JSON
import Data.Map as Map (fromList, (!))
import Control.Monad (mzero)
import Data.Aeson as Aeson
import Get (UserId(..), ReplyToken(..), GroupId(..))
import Data.Maybe (fromJust)

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
  , _evMessage :: Message
  } deriving Show

makeLenses ''LINEEvent



newtype LINEReq = Events { fromLINEReq :: [LINEEvent] } deriving Show


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
    return $ Message mtype (read mid::Int) text

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
    src <- readJSON $ mobj ! "source"
    time <- readJSON $ mobj ! "timestamp"
    mess <- readJSON $ mobj ! "message"
    return $ LINEEvent {
      _evType=typ
      ,_evReplyToken=(ReplyToken tok)
      ,_evSource=src
      ,_evTimeStamp=time
      ,_evMessage=mess
      }

  readJSON _ = mzero

  showJSON ev =
    makeObj [ ("type", showJSON $ ev ^. evType)
           , ("replyToken", showJSON $ unReplyToken $ ev ^. evReplyToken)
           , ("timestamp", showJSON $ ev ^. evTimeStamp)
           , ("message", showJSON $ ev ^. evMessage)
           ]

instance JSON Source where
  readJSON (JSObject obj) = do
    let mobj = Map.fromList $ fromJSObject obj
    uid    <- readJSON$ mobj!"userId"
    typ <- readJSON $ mobj ! "type"
    return $ Source {_srcUserId = Just $ UserId uid, _srcType = typ}

  readJSON _ = mzero
  showJSON src =
    makeObj [ ("userId", showJSON $ fromJust $ unUserId <$> src ^. srcUserId)
           , ("type", showJSON $ src ^. srcType)
           ]



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
    ms <- v .: "message"
    return $ LINEEvent { _evType =t, _evReplyToken=(ReplyToken rt), _evSource=src, _evTimeStamp=ts, _evMessage=ms}

  parseJSON _ = mzero

instance FromJSON Source where
  parseJSON (Object v) = do
    typ <- v .: "type"
    uid <- v .:? "userId"
    gid <- v .:? "groupId"
    return $ Source { _srcUserId = fmap UserId uid, _srcType =typ, _srcGroupId = fmap GroupId gid}

  parseJSON _ = mzero

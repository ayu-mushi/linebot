{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings,ScopedTypeVariables#-}

module Post (Message(Message), msType, msId, msText, LINEEvent(LINEEvent), evType, evReplyToken, evTimeStamp, evMessage, LINEReq(..)) where

import Control.Lens
import Text.JSON
import Data.Map as Map (fromList, (!))
import Control.Monad (mplus, mzero)

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
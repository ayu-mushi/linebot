{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings,ScopedTypeVariables#-}

module Get
( Message(Message)
, msType
, msText
, Reply(Reply)
, repToken
, repMess
, defReply
, defReplyText
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

data Reply = Reply{
  _repToken :: String
  ,_repMess :: [Message]
  } deriving (Show)

makeLenses ''Reply

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
    return $ Reply tok mess'

  readJSON _ = mzero
  showJSON (Reply tok mess) =
    makeObj [ ("replyToken", showJSON tok),
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
    return $ Reply rt ms

instance ToJSON Message where
  toJSON v = object
    ["type" .= (v ^. msType),
     "text" .= (v ^. msText)]

instance ToJSON Reply where
  toJSON v = object ["replyToken" .= (v ^. repToken),
                     "messages" .= (v ^. repMess)]

defMessage :: Message
defMessage = Message "text" ""

defReply :: String -> Reply
defReply token = Reply token [defMessage]

defReplyText :: String -> String -> Reply
defReplyText token text = Reply token [Message "text" text]

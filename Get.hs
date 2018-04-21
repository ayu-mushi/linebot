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
import Control.Lens
import Text.JSON
import Control.Monad (mplus, mzero)

data Message = Message {
  _msType :: String
  , _msText :: String
  }

makeLenses ''Message

data Reply = Reply{
  _repToken :: String
  ,_repMess :: [Message]
  }

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

defMessage :: Message
defMessage = Message "text" ""

defReply :: String -> Reply
defReply token = Reply token [defMessage]

defReplyText :: String -> String -> Reply
defReplyText token text = Reply token [Message "text" text]

{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import System.Environment
import Data.Monoid((<>))
import Network.HTTP.Types.Status()
import System.Random
import Data.Text.Lazy as Text(pack)


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
    post "/callback" $ do
      b <- body
      text $ Text.pack $ show b
      return ()

  {-Just fixie_basic <- lookupEnv "FIXIE_BASIC"
  Just line_channel_token <- lookupEnv "LINE_ACCESS_TOKEN"
  Just line_channel_secret <- lookupEnv "LINE_CHANNEL_SECRET"

  runSpock port . spockT id $ do
    post "callback" $ do
      b <- body
      let Just result = (b ^? key "result" . _Array)
      for_ result $ \msg -> do
        let
          Just content
            = msg ^? key "content"
          Just from
            = content ^? key "from"
        liftIO $ do
          req <- parseUrl "https://api.line.me/v2/oauth/reply"
          manager <- newManager tlsManagerSettings
          let res = object [
            "to" .= Array [from]
           ,"toChannel" .= Number 1383378250
           , "eventType" .=  String "138311608800106203"
           , "content" .= content
           ]
         req' = req { proxy = Just (Proxy {proxyHost = "velodrome.usefixie.com", proxyPort = 80})
         , method = "POST"
         , requestHeaders = [ ("Content-Type", "application/json; charser=UTF-8")
       , ("X-Line-ChannelID", BS.pack line_channel_id)
       , ("X-Line-ChannelSecret", BS.pack line_channel_secret)
       , ("X-Line-Trusted-User-With-ACL", BS.pack line_channel_mid)
       , ("Proxy-Authorization", BS.pack $ "Basic " ++ fixie_basic)
       ]
       , requestBody = RequestBodyLBS (encode res)
       , ()
         }
          httpLbs req' manager
      text ""-}

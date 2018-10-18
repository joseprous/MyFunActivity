{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import
import qualified Data.Aeson as Aeson
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import Control.Monad.Trans.Writer.Lazy (Writer)
import Data.Monoid (Endo)

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE

import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as TLS
import qualified Data.ByteString.Lazy as L

import Handler.Crypto
import qualified Crypto.PubKey.RSA as RSA
import Network.HTTP.Date
import Data.Text.Encoding as E

import Data.Aeson.Lens
import Control.Lens hiding ((.=))
import Data.ActivityStreams

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

toText :: Value -> B.ByteString
toText v = B.concat $ BL.toChunks $ Aeson.encode v

routeToText :: MonadHandler m => Route (HandlerSite m) -> m Text
routeToText url = do
  r <- getUrlRender
  return $ r url

repActivityJson :: (Monad m) => Value -> Writer (Endo [ProvidedRep m]) ()
repActivityJson val = do
  provideRepType "application/activity+json" $ return val
  provideRepType "application/ld+json" $ return val

getActorJSON :: Text -> IO L.ByteString
getActorJSON url = do
  reqUrl <- HC.parseRequest $ T.unpack url
  manager <- TLS.newTlsManager
  let req = reqUrl {requestHeaders = [("Accept","application/ld+json")]}
  response <- HC.httpLbs req manager
  L.putStr $ HC.responseBody response
  return $ HC.responseBody response

getActorInbox :: Text -> IO (Maybe Text)
getActorInbox url = do
  actor <- getActorJSON url
  let inboxUrl = actor ^? key "inbox"
  case inboxUrl of
    (Just (String s)) -> return $ Just s
    _ -> return Nothing

postToInbox :: RSA.PrivateKey -> (Route App -> Text) -> AS -> Text -> Handler ()
postToInbox pkey render (AS vAct) url = do
  initialRequest <- HC.parseRequest $ T.unpack url
  manager <- TLS.newTlsManager

  let actorUrl = E.encodeUtf8 $ render ActorR
  let targetHost = HC.host initialRequest

  now <- liftIO getCurrentTime
  let hnow = utcToHTTPDate now
  let date = formatHTTPDate hnow
  let signed_string = "(request-target): post /users/hiena/inbox\nhost: my-mastodon-test.scalingo.io\ndate: " ++ date
  -- let signed_string = "(request-target): post /inbox\nhost: " ++ targetHost ++ "\ndate: " ++ date
  sig <- liftIO $ sign pkey signed_string
  let header = "keyId=\"" ++ actorUrl ++ "#main-key\",headers=\"(request-target) host date\",signature=\"" ++ sig ++ "\""

  $logDebug $ "signed_string: " ++ E.decodeUtf8 signed_string
  $logDebug $ "header:" ++ E.decodeUtf8 header

  let request = initialRequest {
        method = "POST",
        requestHeaders = [("Content-Type", "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")
                         ,("Host", targetHost)
                         ,("Date", date)
                         ,("Signature", header)],
        requestBody = RequestBodyLBS $ Aeson.encode vAct
        }
  response <- liftIO $ HC.httpLbs request manager
  let textRespose = LT.toStrict $ LE.decodeUtf8 $ HC.responseBody response
  _ <- runDB $ insert $ Logs { logsMessage = "request: " ++ tshow request ++ " response: " ++ textRespose }
  $logDebug $ "request: " ++ tshow request
  $logDebug $ "response: " ++ textRespose
  return ()


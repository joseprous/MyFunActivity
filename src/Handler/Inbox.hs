{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Inbox where

import Import
import Handler.Common
import Data.ActivityStreams
import Control.Lens hiding ((.=))
import Data.Aeson.Lens

import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as TLS
import qualified Data.ByteString.Lazy as L

import Handler.Crypto
import qualified Crypto.PubKey.RSA as RSA
import Network.HTTP.Date
import Data.Text.Encoding as E

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE


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


getInboxR :: Handler TypedContent
getInboxR = selectRep $ do
    provideRep $ return [shamlet|
<p>Get Inbox
|]

toInbox :: AS -> Inbox
toInbox msg = Inbox {inboxMessage = msg}

toFollowers :: Text -> Followers
toFollowers actor = Followers {followersActor = actor}

sendError :: Handler ()
sendError = sendResponseStatus status405 ("Method Not Allowed" :: Text)

generateAccept :: Text -> Text -> Handler AS
generateAccept actor obj = do
  return $ AS $
    object [ "@context" .= ("https://www.w3.org/ns/activitystreams" :: Text)
           , "type" .= ("Accept" :: Text)
           , "actor" .= actor
           , "object" .= obj
           ]

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

postInboxR :: Handler ()
postInboxR = do
  as@(AS msg) <- requireJsonBody :: Handler AS
  let objType = msg ^? key "type"
  case objType of
    (Just (String s)) -> do
      _ <- (runDB . insert . toInbox) as
      case s of
        "Create" -> do
          sendResponseStatus status201 ("CREATED" :: Text)
        "Update" -> do
          $logDebug $ "Update not implemented"
          sendError
        "Delete" -> do
          $logDebug $ "Delete not implemented"
          sendError
        "Follow" -> do
          let (mactor,mobj) = (msg ^? key "actor",msg ^? key "object")
          case (mactor,mobj) of
            (Just (String actor),(Just (String obj))) -> do
              _ <- (runDB . insert . toFollowers) actor
              act <- generateAccept actor obj
              app <- getYesod
              render <- getUrlRender
              let settings = appSettings app
              let tpkey = appPrivateKey settings
              let pkey = keyFromText tpkey
              minbox <- liftIO $ getActorInbox actor
              case minbox of
                Just url -> postToInbox pkey render act url
                Nothing -> return ()
            _ -> do
              $logDebug $ "can't obtain actor and object in Follow"
              sendError
          return ()
        "Accept" -> do
          $logDebug $ "postInboxR error Accept not implemented: "
          sendError
        "Reject" -> do
          $logDebug $ "postInboxR error Reject not implemented: "
          sendError
        "Add" -> do
          $logDebug $ "postInboxR error Add not implemented: "
          sendError
        "Remove" -> do
          $logDebug $ "postInboxR error Remove not implemented: "
          sendError
        "Like" -> do
          $logDebug $ "postInboxR error Like not implemented: "
          sendError
        "Announce" -> do
          $logDebug $ "postInboxR error Announce not implemented: "
          sendError
        "Undo" -> do
          $logDebug $ "postInboxR error Undo not implemented: "
          sendError
        _ -> do
          $logDebug $ "postInboxR error type: " ++ s
          sendError
    _ -> do
      $logDebug $ "postInboxR error objType: " ++ tshow objType
      sendError


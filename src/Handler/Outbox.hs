{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Outbox where

import Import
import Handler.Common
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Set as S

import Control.Lens hiding ((.=))
import Data.Aeson.Lens as L
import Database.Persist.Sql (fromSqlKey)

import Network.Connection (TLSSettings (..))
-- import qualified Network.HTTP.Conduit as C
-- import           Network.HTTP.Simple
import qualified Network.HTTP.Client as HC
import qualified Data.ByteString.Lazy as L
import qualified Data.Maybe as M

getOutboxR :: Handler TypedContent
getOutboxR = selectRep $ do
  provideRep $ return [shamlet|
<p>Get Outbox
|]

toOutbox :: AS -> Outbox
toOutbox (AS as) = Outbox {outboxMessage = toText as}

toActivities :: AS -> Activities
toActivities (AS as) = Activities {activitiesMessage = toText as}

toNotes :: AS -> Notes
toNotes (AS as) = Notes {notesMessage = toText as}

getNextId :: [Int] -> Int
getNextId [] = 1
getNextId (x:_) = x + 1

isActivity :: Text -> Bool
isActivity "Note" = False
isActivity "Create" = True
isActivity _ = False

toCreate :: AS -> Handler AS
toCreate (AS val) = do
  render <- getUrlRender
  return $ AS $
    object [ "@context" .= ("https://www.w3.org/ns/activitystreams" :: Text)
           , "type" .= ("Create":: Text)
           , "actor" .= render ActorR
           , "object" .= val ]

addId :: Value -> Text -> Value
addId v newId = v & _Object . at "id" ?~ String newId

createNote :: AS -> Handler (Maybe AS)
createNote as@(AS v) = do
  noteId <- runDB $ insert $ toNotes as
  render <- getUrlRender
  let newId = render $ NotesR $ fromSqlKey noteId
  let newMsg = addId v newId
  runDB $ Import.replace noteId $ toNotes $ AS newMsg
  return (Just $ AS newMsg)

createObject :: AS -> Handler (Maybe AS)
createObject (AS msg) = do
  let mObj = msg ^? key "object"
  case mObj of
    (Just obj) -> do
      let objType = obj ^? key "type"
      case objType of
        (Just (String s)) | s == "Note" -> createNote $ AS obj
                          | otherwise -> return Nothing
        _ -> return Nothing
    _ -> return Nothing

processActivity :: AS -> AS -> Text -> AS
processActivity (AS vAct) (AS vObj) newId =
  AS $ addId vAct newId
  & _Object . at "object" ?~ vObj
  & _Object . at "to" .~ (vObj ^? key "to")

createActivity :: AS -> AS -> Handler (AS,Route App)
createActivity act obj = do
  activityId <- runDB $ insert $ toActivities act
  render <- getUrlRender
  let idRoute = ActivitiesR $ fromSqlKey activityId
  let newId = render idRoute
  let newAct = processActivity act obj newId
  runDB $ Import.replace activityId $ toActivities newAct
  return (newAct,idRoute)

getActorJSON :: Text -> IO L.ByteString
getActorJSON url = do
  reqUrl <- HC.parseRequest $ T.unpack url
  manager <- HC.newManager HC.defaultManagerSettings
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

getUrls :: Aeson.Array -> [Text]
getUrls a = toList $ map (\(String s)->s) a

getAudienceKey :: AS -> Text -> IO [Text]
getAudienceKey (AS vAct) keyName = do
  let v = vAct ^? key keyName
  case v of
    (Just (Array l)) -> do
      let urls = getUrls l
      a <- mapM getActorInbox urls
      return $ catMaybes a
    _ -> return []

getAudience :: AS -> IO [Text]
getAudience act = do
  a1 <- getAudienceKey act "to"
  a2 <- getAudienceKey act "cc"
  a3 <- getAudienceKey act "bto"
  a4 <- getAudienceKey act "bcc"
  let audience = (S.toList . S.fromList) $ a1 ++ a2 ++ a3 ++ a4
  return audience

postToInbox :: AS -> Text -> IO ()
postToInbox (AS vAct) url = do
  reqUrl <- HC.parseRequest $ T.unpack url
  manager <- HC.newManager HC.defaultManagerSettings
  let req = reqUrl {
        method = "POST",
        requestHeaders = [("Content-Type","application/json")],
        requestBody = RequestBodyLBS $ Aeson.encode vAct
        }
  _ <- HC.httpLbs req manager
  return ()

handleActivity :: AS -> Handler ()
handleActivity msg = do
  mObj <- createObject msg
  case mObj of
    (Just obj) -> do
      (act,route) <- createActivity msg obj
      audience <- liftIO $ getAudience act
      liftIO $ print $ audience
      liftIO $ mapM_ (postToInbox act) audience
      sendResponseCreated route
    Nothing -> sendError

sendError :: Handler ()
sendError = sendResponseStatus status405 ("Method Not Allowed" :: Text)

postOutboxR :: Handler ()
postOutboxR = do
  as@(AS msg) <- requireJsonBody :: Handler AS
  let objType = msg ^? key "type"
  case objType of
    (Just (String s)) ->
      if isActivity s then
        handleActivity as
      else
        toCreate as >>= handleActivity
    _ -> sendError

handleActivitiesR :: Int64 -> Handler Text
handleActivitiesR n = return $ tshow n

handleNotesR :: Int64 -> Handler Text
handleNotesR n = return $ tshow n
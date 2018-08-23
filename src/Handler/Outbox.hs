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

import Control.Lens hiding ((.=))
import Data.Aeson.Lens as L
import Database.Persist.Sql (fromSqlKey)

import Network.Connection (TLSSettings (..))
import qualified Network.HTTP.Conduit as C

import qualified Data.ByteString.Lazy as L
import Data.Maybe

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

sendOne :: Text -> IO ()
sendOne url = do
  response <- C.simpleHttp $ T.unpack url
  L.putStr response

sendActivity :: AS -> Handler ()
sendActivity (AS vAct) = do
  let v = vAct ^? key "to"
  case v of
    (Just (Array l)) -> do
      let urls = map (\(String s)->s) l
      liftIO $ mapM_ sendOne urls
    _ -> return ()

handleActivity :: AS -> Handler ()
handleActivity msg = do
  mObj <- createObject msg
  case mObj of
    (Just obj) -> do
      (act,route) <- createActivity msg obj
      sendActivity act
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

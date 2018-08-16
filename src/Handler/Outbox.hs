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
import Data.Text as T

import Control.Lens hiding ((.=))
import Data.Aeson.Lens as L
import Database.Persist.Sql (fromSqlKey)

getOutboxR :: Handler TypedContent
getOutboxR = selectRep $ do
  provideRep $ return [shamlet|
<p>Get Outbox
|]
  provideRep $ return $ toActivityJson jsonld
  provideRep $ return $ toLdJson jsonld

jsonld :: Value
jsonld = object
         [ "name" .= name
         , "age" .= age
         ]
         where
           name = "Get" :: Text
           age = 28 :: Int

toOutbox :: AS -> Outbox
toOutbox as = Outbox {outboxMessage = toText $ Aeson.toJSON as}

toActivities :: AS -> Activities
toActivities as = Activities {activitiesMessage = toText $ Aeson.toJSON as}

toNotes :: AS -> Notes
toNotes as = Notes {notesMessage = toText $ Aeson.toJSON as}


getNextId :: [Int] -> Int
getNextId [] = 1
getNextId (x:_) = x + 1

isActivity :: Text -> Bool
isActivity "Note" = False
isActivity "Create" = True
isActivity _ = False

toCreate :: AS -> Handler AS
toCreate (AS val) = do
  actor <- routeToText ActorR
  return $ AS $
    object [ "@context" .= ("https://www.w3.org/ns/activitystreams" :: Text)
           , "type" .= ("Create":: Text)
           , "actor" .= actor
           , "object" .= val ]

createNote :: AS -> Handler (Maybe AS)
createNote msg = do
  noteId <- runDB $ insert $ toNotes msg
  let idNumber = fromSqlKey noteId
  idProp <- routeToText $ NotesR idNumber
  let newMsg = AS $ (Aeson.toJSON msg) & _Object . at "id"  ?~ String idProp
  runDB $ Import.replace noteId $ toNotes newMsg
  return (Just newMsg)

createObject :: AS -> Handler (Maybe AS)
createObject msg = do
  let mObj = (Aeson.toJSON msg) ^? key "object" . L.nonNull
  case mObj of
    (Just obj) -> do
      liftIO $ print obj
      let objType = obj ^? key "type" . L.nonNull
      case objType of
        (Just (String s)) | s == "Note" -> createNote $ AS obj
                          | otherwise -> return Nothing
        _ -> return Nothing
    _ -> return Nothing

createActivity :: AS -> AS -> Handler ()
createActivity act@(AS vAct) obj@(AS vObj) = do
  activityId <- runDB $ insert $ toActivities act
  liftIO $ print $ activityId
  let idNumber = fromSqlKey activityId
  liftIO $ print $ idNumber
  idProp <- routeToText $ ActivitiesR idNumber
  let actWithId = vAct & _Object . at "id"  ?~ String idProp
  liftIO $ print actWithId
  let actWithObj = actWithId & _Object . at "object"  ?~ vObj
  liftIO $ print actWithObj
  runDB $ Import.replace activityId $ toActivities $ AS actWithObj
  sendResponseCreated $ ActivitiesR idNumber

handleActivity :: AS -> Handler ()
handleActivity msg = do
  liftIO $ print $ msg
  mObj <- createObject msg
  case mObj of
    (Just obj) -> createActivity msg obj
    Nothing -> sendError

sendError :: Handler ()
sendError = sendResponseStatus status405 ("Method Not Allowed" :: Text)

postOutboxR :: Handler ()
postOutboxR = do
  msg <- requireJsonBody :: Handler AS
  let objType = (Aeson.toJSON msg) ^? key "type" . L.nonNull
  liftIO $ print $ objType
  case objType of
    (Just (String s)) -> if isActivity s then handleActivity msg else do
      act <- toCreate msg
      handleActivity act
    _ -> sendError

handleActivitiesR :: Int64 -> Handler Text
handleActivitiesR n = return $ T.pack $ show n

handleNotesR :: Int64 -> Handler Text
handleNotesR n = return $ T.pack $ show n

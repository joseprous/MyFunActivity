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

import Handler.Crypto

getInboxR :: Handler TypedContent
getInboxR = do
  render <- getUrlRender
  inbox <- runDB $ selectList [] [Desc InboxId]
  let messages = map (\((Entity _ i))-> inboxMessage i) inbox
  let inboxUrl = render InboxR
  let totalItems = length messages
  let jsonld = object
        [ "@context" .= ("https://www.w3.org/ns/activitystreams" :: Text)
        , "type" .= ("OrderedCollection" :: Text)
        , "id" .= inboxUrl
        , "totalItems" .= totalItems
        , "orderedItems" .= messages
        ]
  selectRep $ do
    provideRep $ return [shamlet|
<p>Get Inbox
|]
    repActivityJson jsonld

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
                (Just url) -> postToInbox pkey render act url
                Nothing -> return ()
            _ -> do
              $logDebug $ "can't obtain actor and object in Follow"
              sendError
          return ()
        "Accept" -> do
          let (mactor,mobj) = (msg ^? key "actor",msg ^? key "object")
          case (mactor,mobj) of
            (Just (String actor),(Just (String _))) -> do
              maybeActor <- runDB $ getBy $ UniqueFollowingActor actor
              case maybeActor of
                (Just (Entity k a)) | (followingActor a) == actor -> do
                                        _ <- runDB $ update k [FollowingAccepted =. True]
                                        return ()
                                    | otherwise -> return ()
                _ -> return ()
              return ()
            _ -> do
              $logDebug $ "can't obtain actor and object in Accept"
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


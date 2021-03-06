{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Actor where

import Import
import Handler.Common

getActorJson :: AppSettings -> (Route App -> Text) -> Value
getActorJson settings render =
  let actorUrl = render ActorR
      inboxUrl = render InboxR
      outboxUrl = render OutboxR
      user = appMyUser settings
      key = appPublicKey settings
      avatar = appAvatar settings
  in object
     [ "@context" .= ([ "https://www.w3.org/ns/activitystreams"
                      , "https://w3id.org/security/v1"
                      ] :: [Text])
     , "id" .= actorUrl
     , "type" .= ("Person" :: Text)
     , "preferredUsername" .= user
     , "inbox" .= inboxUrl
     , "outbox" .= outboxUrl
     , "publicKey" .= object
       [ "id" .= (actorUrl ++ "#main-key")
       , "owner" .= actorUrl
       , "publicKeyPem" .= key
       ]
     , "icon" .= object
       [ "type" .= ("Image" :: Text)
       , "mediaType" .= ("image/jpeg" :: Text)
       , "url" .= avatar
       ]
     ]

getActorR :: Handler TypedContent
getActorR = do
  app <- getYesod
  render <- getUrlRender
  let jsonld = getActorJson (appSettings app) render
  selectRep $ do
    provideRep $ return [shamlet|
<p>Get Actor
|]
    repActivityJson jsonld


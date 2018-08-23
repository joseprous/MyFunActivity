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
      user = appMyUser settings
      key = appPublicKey settings
  in getActorJson' actorUrl inboxUrl user key

getActorJson' :: Text -> Text -> Text -> Text -> Value
getActorJson' actorUrl inboxUrl user key =
  object
  [ "@context" .= ([ "https://www.w3.org/ns/activitystreams"
                   , "https://w3id.org/security/v1"
                   ] :: [Text])
  , "id" .= actorUrl
  , "type" .= ("Person" :: Text)
  , "preferredUsername" .= user
  , "inbox" .= inboxUrl
  , "publicKey" .= object
    [ "id" .= (actorUrl ++ "#main-key")
    , "owner" .= actorUrl
    , "publicKeyPem" .= key
    ]
  , "links" .= ([object
                  [ "rel" .= ("self" :: Text)
                  , "type" .= ("application/activity+json" :: Text)
                  , "href" .= actorUrl
                  ]
                ] :: [Value])
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


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

getActorJson :: Handler Value
getActorJson = do
  App {..} <- getYesod
  actorUrl <- routeToText ActorR
  inboxUrl <- routeToText InboxR
  let user = appMyUser appSettings
  let key = appPublicKey appSettings
  return $ object
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
  jsonld <- getActorJson
  selectRep $ do
    provideRep $ return [shamlet|
<p>Get Actor
|]
    provideRep $ return $ toActivityJson jsonld
    provideRep $ return $ toLdJson jsonld


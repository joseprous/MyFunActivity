{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Actor where

import Import
import Handler.Common

getActorR :: Handler TypedContent
getActorR = selectRep $ do
    provideRep $ return [shamlet|
<p>Get Actor
|]
    provideRep $ return $ toActivityJson jsonld
    provideRep $ return $ toLdJson jsonld
  where
    jsonld :: Value
    jsonld = object
        [ "@context" .= ([ "https://www.w3.org/ns/activitystreams"
                         , "https://w3id.org/security/v1"
                         ] :: [Text])
        , "id" .= actorUrl
        , "type" .= ("Person" :: Text)
        , "preferredUsername" .= myUser
        , "inbox" .= inboxUrl
        , "publicKey" .= object
          [ "id" .= (actorUrl ++ "#main-key")
          , "owner" .= actorUrl
          , "publicKeyPem" .= publicKey
          ]
        , "links" .= ([object
                       [ "rel" .= ("self" :: Text)
                       , "type" .= ("application/activity+json" :: Text)
                       , "href" .= actorUrl
                       ]
                      ] :: [Value])
        ]
    actorUrl = "https://" ++ myHost ++ "/actor"
    inboxUrl = "https://" ++ myHost ++ "/inbox"
    myUser = appMyUser compileTimeAppSettings
    myHost = appMyHost compileTimeAppSettings
    publicKey = appPublicKey compileTimeAppSettings


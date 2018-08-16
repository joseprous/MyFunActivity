{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Webfinger where

import Import
import Handler.Common

getWebfingerR :: Handler Value
getWebfingerR = do
  let myUser = appMyUser compileTimeAppSettings
  home <- routeToText HomeR
  let subject = myUser ++ "@" ++ home
  actorUrl <- routeToText ActorR
  return $ object
        [ "subject" .= subject
        , "links" .= ([object
                       [ "rel" .= ("self" :: Text)
                       , "type" .= ("application/activity+json" :: Text)
                       , "href" .= actorUrl
                       ]
                      ] :: [Value])
        ]

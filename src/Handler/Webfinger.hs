{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Webfinger where

import Import
import Handler.Common

getWebfingerR :: Handler Value
getWebfingerR = do
  App {..} <- getYesod
  let myUser = appMyUser appSettings
  let myHost = "my-fun-activity.herokuapp.com"
  -- home <- routeToText ActorR
  let subject = "acct:" ++ myUser ++ "@" ++ myHost
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

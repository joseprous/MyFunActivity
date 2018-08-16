{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Webfinger where

import Import

getWebfingerR :: Handler Value
getWebfingerR = return $ object
        [ "subject" .= subject
        , "links" .= ([object
                       [ "rel" .= ("self" :: Text)
                       , "type" .= ("application/activity+json" :: Text)
                       , "href" .= actorUrl
                       ]
                      ] :: [Value])
        ]
  where
    subject = myUser ++ "@" ++ myHost
    actorUrl = "https://" ++ myHost ++"/actor"
    myUser = appMyUser compileTimeAppSettings
    myHost = appMyHost compileTimeAppSettings

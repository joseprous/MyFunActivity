{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Followers where

import Import
import Handler.Common

getFollowersR :: Handler TypedContent
getFollowersR = do
  render <- getUrlRender
  followers <- runDB $ selectList [] [Desc FollowersId]
  let actors = map (\((Entity _ f))-> followersActor f) followers
  let followersUrl = render FollowersR
  let totalItems = length actors
  let jsonld = object
        [ "@context" .= ("https://www.w3.org/ns/activitystreams" :: Text)
        , "type" .= ("OrderedCollection" :: Text)
        , "id" .= followersUrl
        , "totalItems" .= totalItems
        , "orderedItems" .= actors
        ]
  selectRep $ do
    provideRep $ return [shamlet|
<p>Get Followers
|]
    repActivityJson jsonld

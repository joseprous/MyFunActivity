{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Following where

import Import
import Handler.Common

getFollowingR :: Handler TypedContent
getFollowingR = do
  render <- getUrlRender
  following <- runDB $ selectList [] [Desc FollowingId]
  let actors = map (\((Entity _ f))-> followingActor f) following
  let followingUrl = render FollowingR
  let totalItems = length actors
  let jsonld = object
        [ "@context" .= ("https://www.w3.org/ns/activitystreams" :: Text)
        , "type" .= ("OrderedCollection" :: Text)
        , "id" .= followingUrl
        , "totalItems" .= totalItems
        , "orderedItems" .= actors
        ]
  selectRep $ do
    provideRep $ return [shamlet|
<p>Get Following
|]
    repActivityJson jsonld

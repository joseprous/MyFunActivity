{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Outbox where

import Import
import Handler.Common

getOutboxR :: Handler TypedContent
getOutboxR = selectRep $ do
  provideRep $ return [shamlet|
<p>Get Outbox
|]
  provideRep $ return $ toActivityJson jsonld
  provideRep $ return $ toLdJson jsonld

jsonld :: Value
jsonld = object
         [ "name" .= name
         , "age" .= age
         ]
         where
           name = "Get" :: Text
           age = 28 :: Int

postOutboxR :: Handler TypedContent
postOutboxR = selectRep $ do
    provideRep $ return [shamlet|
<p>Post Outbox
|]
    provideRep $ return $ toActivityJson o
    provideRep $ return $ toLdJson o
  where
    o = object
        [ "name" .= name
        , "age" .= age
        ]
    name = "Post" :: Text
    age = 28 :: Int

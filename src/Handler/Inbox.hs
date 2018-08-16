{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Inbox where

import Import
import Handler.Common
import qualified Data.Aeson as Aeson

getInboxR :: Handler TypedContent
getInboxR = selectRep $ do
    provideRep $ return [shamlet|
<p>Get Inbox
|]

toInbox :: AS -> Inbox
toInbox as = Inbox {inboxMessage = toText $ Aeson.toJSON as}

postInboxR :: Handler ()
postInboxR = do
  msg@(AS val) <- requireJsonBody :: Handler AS
  _ <- (runDB . insert . toInbox) msg
  sendResponseStatus status201 ("CREATED" :: Text)

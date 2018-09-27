{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Inbox where

import Import
import Handler.Common
import Data.ActivityStreams

getInboxR :: Handler TypedContent
getInboxR = selectRep $ do
    provideRep $ return [shamlet|
<p>Get Inbox
|]

toInbox :: AS -> Inbox
toInbox msg = Inbox {inboxMessage = msg}

postInboxR :: Handler ()
postInboxR = do
  _ <- requireJsonBody >>= runDB . insert . toInbox
  sendResponseStatus status201 ("CREATED" :: Text)


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import
import qualified Data.Aeson as Aeson
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")


data ActivityJson = ActivityJson Value
data LdJson = LdJson Value

mimeTypeActivity :: ContentType
mimeTypeActivity = "application/activity+json"

instance ToContent ActivityJson where
    toContent (ActivityJson a) = toContent $ a
instance ToTypedContent ActivityJson where
    toTypedContent = TypedContent mimeTypeActivity . toContent
instance HasContentType ActivityJson where
    getContentType _ = mimeTypeActivity

mimeTypeLd :: ContentType
mimeTypeLd = "application/ld+json"

instance ToContent LdJson where
    toContent (LdJson a) = toContent $ a
instance ToTypedContent LdJson where
    toTypedContent = TypedContent mimeTypeLd . toContent
instance HasContentType LdJson where
    getContentType _ = mimeTypeLd

toActivityJson :: Value -> ActivityJson
toActivityJson v = ActivityJson v

toLdJson :: Value -> LdJson
toLdJson v = LdJson v

data AS = AS Aeson.Value deriving (Show)

instance ToJSON AS where
  toJSON (AS v) = v

instance FromJSON AS where
  parseJSON v = return $ AS v

toText :: Value -> B.ByteString
toText v = B.concat $ BL.toChunks $ Aeson.encode v

routeToText :: MonadHandler m => Route (HandlerSite m) -> m Text
routeToText url = do
  r <- getUrlRender
  return $ r url

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
import Control.Monad.Trans.Writer.Lazy (Writer)
import Data.Monoid (Endo)

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

toText :: Value -> B.ByteString
toText v = B.concat $ BL.toChunks $ Aeson.encode v

routeToText :: MonadHandler m => Route (HandlerSite m) -> m Text
routeToText url = do
  r <- getUrlRender
  return $ r url

repActivityJson :: (Monad m) => Value -> Writer (Endo [ProvidedRep m]) ()
repActivityJson val = do
  provideRepType "application/activity+json" $ return val
  provideRepType "application/ld+json" $ return val

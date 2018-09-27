{-# LANGUAGE OverloadedStrings          #-}

module Data.ActivityStreams where

import Database.Persist.Sql
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy          as BL

data AS = AS Aeson.Value deriving (Show, Eq)

instance Aeson.ToJSON AS where
  toJSON (AS v) = v

instance Aeson.FromJSON AS where
  parseJSON v = return $ AS v

instance PersistField AS where
  toPersistValue v = PersistText $ TE.decodeUtf8 $ BL.toStrict $ Aeson.encode v
  fromPersistValue (PersistByteString v)
    = case Aeson.decode (BL.fromStrict v) of
        Nothing -> Left "Invalid jsonb"
        Just j  -> Right j
  fromPersistValue _ = Left "Invalid PersistValue for JSONB. PersistByteString required."

instance PersistFieldSql AS where
   sqlType _ = SqlOther "JSONB"

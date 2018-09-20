{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Crypto where

import Import

import Data.PEM
import qualified Data.ByteString as B
import Data.X509.Memory
import Data.X509
import Data.List as L
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as PKCS15
import Crypto.Hash.Algorithms
import Crypto.Random.Types
import qualified Data.ByteString.Base64 as B64
import Data.Text.Encoding as E

readPEMFile :: FilePath -> IO [PEM]
readPEMFile file = do
    content <- B.readFile file
    return $ either error id $ pemParseBS content

keyFromText :: Text -> RSA.PrivateKey
keyFromText tcontent = let content = E.encodeUtf8 tcontent
                           (Right [pem]) = pemParseBS content
                           (PrivKeyRSA rsaKey) = getKey pem
                       in rsaKey

getKey :: PEM -> PrivKey
getKey pem = L.head $ catMaybes $ pemToKey [] pem

readKeyFile :: FilePath -> IO RSA.PrivateKey
readKeyFile file = do
  (pem:[]) <- readPEMFile file
  let (PrivKeyRSA k) = getKey pem
  return k

sign :: MonadRandom m => RSA.PrivateKey -> ByteString -> m ByteString
sign key message = do
  (Right m) <- PKCS15.signSafer (Just SHA256) key message
  return $ B64.encode m

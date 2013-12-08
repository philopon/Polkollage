{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, PackageImports, TupleSections #-}

module Common where

import           Snap.Core
import qualified Data.ByteString as S
import qualified Data.Aeson as JSON
import           Data.Monoid
------------------------------------------------------------------------------

writeJSON :: (MonadSnap m, JSON.ToJSON a) => a -> m ()
writeJSON json = do
  modifyResponse (setContentType "application/json")
  writeLBS $ JSON.encode json

notFound, badRequest :: MonadSnap m => m ()
notFound   = modifyResponse (setResponseCode 404) >> writeBS "Not Found\n"
badRequest = modifyResponse (setResponseCode 400) >> writeBS "Bad Request\n"

(</>) :: S.ByteString -> S.ByteString -> S.ByteString
d </> f | S.null d  = f
        | S.null f  = d
        | otherwise = case (S.last d == slash, S.head f == slash) of
          (True,  True ) -> d <> S.tail f
          (False, False) -> d <> S.cons slash f
          _              -> d <> f
  where slash = 47


{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, PackageImports, TupleSections #-}

module Common where

import           Snap.Core
import qualified Data.ByteString as S
import qualified Data.Text as T
import qualified Data.Aeson as JSON
import           Data.Monoid
import           Snap.Snaplet.Heist
import Heist
import           Text.XmlHtml(Node(..))
import Snap
------------------------------------------------------------------------------

writeJSON :: (MonadSnap m, JSON.ToJSON a) => a -> m ()
writeJSON json = do
  modifyResponse (setContentType "application/json")
  writeLBS $ JSON.encode json

notFound, badRequest :: HasHeist b => Handler b v ()
notFound     = renderError 404 "Not Found."
badRequest   = renderError 400 "Bad Request."


renderError :: HasHeist b => Int -> T.Text -> Handler  b v ()
renderError code message = do
  modifyResponse (setResponseCode code)
  renderWithSplices "error" (("code"    ## return [TextNode . T.pack $ show code]) <>
                             ("message" ## return [TextNode  message]))


(</>) :: S.ByteString -> S.ByteString -> S.ByteString
d </> f | S.null d  = f
        | S.null f  = d
        | otherwise = case (S.last d == slash, S.head f == slash) of
          (True,  True ) -> d <> S.tail f
          (False, False) -> d <> S.cons slash f
          _              -> d <> f
  where slash = 47


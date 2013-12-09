{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, PackageImports, TupleSections, TemplateHaskell, ScopedTypeVariables #-}

module Handler.Image(install) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple
import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString.Lazy  as L
------------------------------------------------------------------------------
import           Application
import           Common
import qualified Data.Text   as T
import           Data.Monoid
import           Heist
import           Snap.Snaplet.Heist
import           Text.XmlHtml(Node(..))
import Query
import           Data.Digest.Pure.SHA
import qualified Data.Aeson as JSON
import Control.Monad.CatchIO
import Control.Exception hiding(catch)

import "mtl" Control.Monad.Trans

install :: ByteString -> Initializer App App ()
install pfx = addRoutes [ (pfx,                method GET getImage <|> method DELETE deleteImage)
                        , (pfx </> "/:id",     method GET getImage <|> method DELETE deleteImage)
                        , (pfx </> "/png/:id", method GET getPng)
                        ]

getImage :: AppHandler ()
getImage = do
  Just (ident,_) <- ((\mbi -> mbi >>= SC.readInteger) <$> getParam "id")
  dbResult       <-
    ( with db . withTransaction $ do
         orig  <- Query.selectOriginalFromImagesById (fromIntegral ident)
         (w,h) <- Query.selectSmallSizeFromRawImagesById orig
         return $ Right (orig, w, h)
    ) `catch` (\(e::SomeException) -> return $ Left e)

  case dbResult of
    Right (orig, w, h) -> 
      renderWithSplices "image" (("ident"    ## return [TextNode . T.pack $ show ident]) <>
                                 ("original" ## return [TextNode . T.pack $ show orig]) <>
                                 ("width"    ## return [TextNode . T.pack $ show (w::Int)]) <>
                                 ("height"   ## return [TextNode . T.pack $ show (h::Int)])
                                )
    Left _ -> notFound

getPng :: AppHandler ()
getPng = do
  Just (ident,_) <- (\mbi -> mbi >>= SC.readInteger) <$> getParam "id"
  png            <- Query.selectImageFromImagesById (fromIntegral ident)
  modifyResponse (setContentType "image/png")
  writeLBS png

deleteImage :: AppHandler ()
deleteImage = do
  Just (ident',_) <- (\mbi -> mbi >>= SC.readInteger) <$> getParam "id"
  Just pass       <- getParam "deletePassword"
  let ident = fromIntegral ident'
  with db $ do
    (key,salt) <- selectDeleteKeyById ident
    if (bytestringDigest . sha1 $ L.fromChunks [pass,salt]) == L.fromStrict key
      then do i <- deleteImageById ident
              writeJSON $ JSON.object ["deleted" JSON..= i]
      else modifyResponse (setResponseCode 401) >>
           writeJSON (JSON.object ["error" JSON..= ("Unauthorized"::String)])



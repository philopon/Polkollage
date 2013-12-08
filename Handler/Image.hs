{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, PackageImports, TupleSections, TemplateHaskell #-}

module Handler.Image(install) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple
import qualified Data.ByteString.Char8 as SC
------------------------------------------------------------------------------
import           Application
import           Common
import qualified Data.Text   as T
import           Data.Monoid
import           Data.Int
import           Heist
import           Snap.Snaplet.Heist
import           Text.XmlHtml(Node(..))

install :: ByteString -> Initializer App App ()
install pfx = addRoutes [ (pfx </> "/:id",     method GET getImage)
                        , (pfx </> "/png/:id", method GET getPng)
                        ]

getImage :: AppHandler ()
getImage = do
  Just (ident,_) <- (\mbi -> mbi >>= SC.readInteger) <$> getParam "id"
  (orig, w, h)  <- with db $ do
    (Only orig):_  <- query "SELECT original FROM images WHERE id = ?;" (Only ident)
    (w,h):_        <- query "SELECT small_width, small_height FROM raw_images WHERE id = ?;" (Only orig)
    return (orig, w, h)
  renderWithSplices "image" (("ident"    ## return [TextNode . T.pack $ show ident]) <>
                             ("original" ## return [TextNode . T.pack $ show (orig::Int64)]) <>
                             ("width"    ## return [TextNode . T.pack $ show (w::Int)]) <>
                             ("height"   ## return [TextNode . T.pack $ show (h::Int)])
                            )

getPng :: AppHandler ()
getPng = do
  Just (ident,_)        <- (\mbi -> mbi >>= SC.readInteger) <$> getParam "id"
  (Only (Binary png)):_ <- with db $ query "SELECT data FROM images WHERE id = ?;" (Only ident)
  modifyResponse (setContentType "image/png")
  writeLBS png

-- CREATE TABLE raw_images(id bigserial PRIMARY KEY, sha1 bytea NOT NULL UNIQUE, data bytea NOT NULL, width integer NOT NULL, height integer NOT NULL, small bytea NOT NULL, small_width integer NOT NULL, small_height integer NOT NULL, scale double precision NOT NULL);

-- CREATE table images (id bigserial PRIMARY KEY, original bigint NOT NULL REFERENCES raw_images(id), data bytea NOT NULL, color color NOT NULL, circles arc[] NOT NULL);

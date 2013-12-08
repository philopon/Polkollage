{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, PackageImports, TupleSections, TemplateHaskell #-}

module Handler.Edit(install) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import "mtl"     Control.Monad.Trans
import           Data.ByteString (ByteString)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple
import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString.Lazy as L
import qualified Data.Aeson as JSON

------------------------------------------------------------------------------
import           Application
import           Common
import           Graphics.Rendering.Cairo
import           Cairo
import qualified Data.Vector as V
import Data.Int
import Data

install :: ByteString -> Initializer App App ()
install pfx = addRoutes [(pfx </> "", method GET getImage <|> method POST postImage)]

getImage :: AppHandler ()
getImage = do
  modifyResponse (setContentType "image/png")
  (_,dat, _, _) <- createImage True
  writeLBS dat

postImage :: AppHandler ()
postImage = do
  (ident, dat, circles, color) <- createImage False
  ret:_ <- with db $ query
           "INSERT INTO images(original, data, color, circles) VALUES (?,?,?,?) RETURNING id"
           (ident, Binary dat, color, V.fromList circles)
  writeJSON $ JSON.object ["id" JSON..= (fromOnly ret::Int64)]

createImage :: Bool -> AppHandler (Int64, L.ByteString, [Circle], Color)
createImage isPreview = do
  Just (ident,_) <- (\mbp -> mbp >>= SC.readInteger) <$> getParam "ident"
  Just circles   <- (\mbp -> join $ mbp >>= JSON.decode . L.fromStrict) <$> getParam "data"
  Just color     <- (\mbp -> join $ mbp >>= JSON.decode . L.fromStrict) <$> getParam "color"

  (d, s):_ <- with db $
              if isPreview
              then query "SELECT small, scale FROM raw_images WHERE id = ?;" (Only ident)
              else query "SELECT data,  scale FROM raw_images WHERE id = ?;" (Only ident)
  let sCircles = if isPreview
                 then circles
                 else map (scaleCircle s) circles
  dat <- liftIO $ renderImage color sCircles (fromBinary d)
  return (fromIntegral ident,dat, circles, color)

renderImage :: Color -> [Circle] -> ByteString -> IO L.ByteString
renderImage color circles dat = withImageSurfaceFromPNGData dat $ \img -> do
  w <- imageSurfaceGetWidth  img
  h <- imageSurfaceGetHeight img

  withImageSurfaceFromPNGData dat $ \surf -> do
    renderWith surf $ do
      setSourceRGBA (red color) (green color) (blue color) (alpha color)
      rectangle 0 0 (fromIntegral w) (fromIntegral h)
      fill
      setSourceSurface img 0 0
      mapM_ drawCircle circles
    surfaceWriteToPNGData surf

scaleCircle :: Double -> Circle -> Circle
scaleCircle s = (left *~ s) . (top *~ s) . (scaleX *~ s) . (scaleY *~ s)

drawCircle :: Circle -> Render ()
drawCircle c = do
  save
  translate (_left c)   (_top c)
  rotate    (_angle c * pi / 180)
  scale     (_scaleX c) (_scaleY c)
  arc 0 0   (_radius c) 0 (2*pi)
  fill
  restore

--------------------------------------------------------------------------------

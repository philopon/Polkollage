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
import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString       as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Aeson as JSON

------------------------------------------------------------------------------
import           Application
import           Polkollage.Common
import           Graphics.Rendering.Cairo
import           Polkollage.Cairo
import           Polkollage.Data
import qualified Polkollage.Query as Query
import qualified System.Random.MWC as MWC
import           Data.Digest.Pure.SHA
import qualified Data.Vector.Unboxed as U

install :: ByteString -> Initializer App App ()
install pfx = addRoutes [(pfx </> "", method GET getImage <|> method POST postImage)]

getImage :: AppHandler ()
getImage = do
  modifyResponse (setContentType "image/png")
  (_,dat, _, _) <- createImage True
  writeLBS dat

postImage :: AppHandler ()
postImage = do
  (orig, dat, circles, color) <- createImage False
  Just pwd <- getParam "deletePassword"
  salt     <- liftIO genSalt
  let storePass = bytestringDigest . sha1 $ L.fromChunks [pwd, salt] :: L.ByteString

  ident <- with db $ Query.insertImage orig dat color circles storePass salt
  writeJSON $ JSON.object ["id" JSON..= ident]

genSalt :: IO S.ByteString
genSalt = MWC.withSystemRandom . MWC.asGenIO $ \gen -> do
  v <- S.pack . U.toList <$> MWC.uniformVector gen 20
  return v
  

createImage :: Bool -> AppHandler (Int, L.ByteString, [Circle], Color)
createImage isPreview = do
  Just (ident',_) <- (\mbp -> mbp >>= SC.readInteger)                    <$> getParam "ident"
  Just circles    <- (\mbp -> join $ mbp >>= JSON.decode . L.fromStrict) <$> getParam "data"
  Just color      <- (\mbp -> join $ mbp >>= JSON.decode . L.fromStrict) <$> getParam "color"
  let ident = fromIntegral ident'
  (d, s) <- with db $ Query.getImageFromRawImagesById isPreview ident
  let sCircles = if isPreview
                 then circles
                 else map (scaleCircle s) circles
  dat <- liftIO $ renderImage color sCircles d
  return (ident, dat, circles, color)

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


{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, PackageImports, TupleSections #-}

module Handler.Raw(install) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import "mtl"     Control.Monad.Trans
import           Data.ByteString (ByteString)
import qualified Data.Text.Encoding as T
import           Data.Int(Int64)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session
import           Snap.Util.FileUploads
import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString.Lazy  as L
import qualified Snap.Iteratee as Iter
import qualified Data.Aeson as JSON
import           Data.Aeson((.=))
------------------------------------------------------------------------------
import           Application
import           Data.Digest.Pure.SHA
import           Data.Maybe
import           Common
import           Graphics.GD

-- CREATE TABLE raw_images(id bigserial PRIMARY KEY, sha1 bytea NOT NULL UNIQUE, data bytea NOT NULL, width integer NOT NULL, height integer NOT NULL, small bytea NOT NULL, small_width integer NOT NULL, small_height integer NOT NULL, scale double precision NOT NULL);

install :: ByteString -> Initializer App App ()
install pfx = addRoutes
              [ (pfx </> "",     method GET getRaw <|> method POST postRaw)
              , (pfx </> "/:id", method GET getRaw)
              ]

getRaw :: AppHandler ()
getRaw = getParam "id" >>= \idprm -> case idprm of
  Nothing  -> badRequest
  Just ids -> case fromIntegral . fst <$> SC.readInteger ids :: Maybe Int64 of
    Nothing   -> badRequest
    Just idnt -> 
      with db $ query "SELECT small FROM raw_images WHERE id = ?;" (Only idnt) >>= \q ->
      case q of
        []                    -> notFound
        (Only (Binary dat)):_ -> do
          modifyResponse (setContentType "image/png")
          writeLBS dat

postRaw :: AppHandler ()    
postRaw = do
  htmlToken <- getParam "token"
  sessToken <- with sess $ do
    token <- getFromSession "token"
    deleteFromSession "token"
    commitSession
    return (fmap T.encodeUtf8 token)

  when (htmlToken /= sessToken) $ do
    modifyResponse $ setResponseCode 401
    writeBS "session timeout\n"
    getResponse >>= finishWith

  posted <- handleMultipart defPolicy policy

  case catMaybes posted of
    []                         -> fail "no image field"
    (hash, ct, dat):_ -> do
      idnt <- with db $ do
        r <- query "SELECT id FROM raw_images WHERE sha1 = ?" (Only $ Binary hash)
        if not $ null r
          then return (Left . fromOnly $ head r)
          else do
          (raw, thumb, width, height, thW, thH, scale) <- liftIO $ loadImage ct (L.toStrict dat)
          Right . fromOnly . head <$>
            query "INSERT INTO raw_images(sha1, data, width, height, small, small_height, small_width, scale) VALUES (?,?,?,?,?,?,?,?) RETURNING id"
            (Binary hash, Binary raw, width, height, Binary thumb, thH, thW, scale)

      writeJSON $ JSON.object
        [ "id"          .= (either id id idnt :: Int64)
        , "status-code" .= (either (const 1) (const 0) idnt :: Int)
        , "status"      .= (either (const "Already exists") (const "Uploaded") idnt :: ByteString)
        ]

  where
    defPolicy = setMaximumFormInputSize (8 * 1024 * 1024) $ defaultUploadPolicy
    policy PartInfo{partFieldName = "image", partContentType = ct} =
      if ct `elem` ["image/jpeg", "image/png", "image/gif"]
      then do
        dat <- L.fromChunks <$> Iter.consume
        return (Just (bytestringDigest $ sha1 dat, ct, dat))
      else return Nothing
    policy _ = return Nothing

loadImage :: ByteString -> ByteString -> IO (ByteString, ByteString, Int, Int, Int, Int, Double)
loadImage ct blob = do
  image <- case ct of
    "image/jpeg" -> loadJpegByteString blob
    "image/png"  -> loadPngByteString  blob
    "image/gif"  -> loadGifByteString  blob
    _            -> fail "not compatible content-type"
  raw             <- savePngByteString image
  (width, height) <- imageSize image
  let (scale, w', h') = thumbnailSize 800 600 width height
  small           <- savePngByteString =<< resizeImage w' h' image
  return (raw, small, width, height, w', h', scale)

thumbnailSize :: Int -> Int -> Int -> Int -> (Double, Int, Int)
thumbnailSize rw rh w h =
  let sw = (fromIntegral w / fromIntegral rw) :: Double
      h' = fromIntegral h / sw
  in if h' <= fromIntegral rh
     then (sw, max 1 rw, max 1 $ floor h')
     else let sh =  (fromIntegral h / fromIntegral rh) :: Double
              w' = fromIntegral w / sh
          in (sh, max 1 $ floor w', max 1 $ rh)


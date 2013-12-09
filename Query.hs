{-#LANGUAGE OverloadedStrings#-}
module Query where

import Control.Monad
import Snap.Snaplet.PostgresqlSimple
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as S
import Snap(MonadSnap)
import qualified Data.Vector as V
import Data
import Data.Int(Int64)

getByUnique :: Monad m => m [b] -> m b
getByUnique q = q >>= \r -> case r of
  []  -> fail "not found"
  f:_ -> return f

--
-- for Raw.hs
--

selectSmallImageFromRawImagesById :: (HasPostgres m, MonadSnap m) => Int -> m L.ByteString
selectSmallImageFromRawImagesById idnt =
  (fromBinary . fromOnly) `liftM`
  getByUnique (query "SELECT small FROM raw_images WHERE id = ?;" (Only idnt))

selectIdFromRawImagesBySha1 :: (HasPostgres m, MonadSnap m) => L.ByteString -> m Int
selectIdFromRawImagesBySha1 sha =
  fromOnly `liftM`
  getByUnique (query "SELECT id FROM raw_images WHERE sha1 = ?" (Only $ Binary sha))

insertRawImage :: HasPostgres m => L.ByteString -> S.ByteString -> Int -> Int -> S.ByteString -> Int -> Int -> Double -> m Int
insertRawImage hash raw width height small w' h' scale = do
  ident <- getByUnique $
           query "INSERT INTO raw_images(sha1, data, width, height, small, small_width, small_height, scale) VALUES (?,?,?,?,?,?,?,?) RETURNING id"
           (Binary hash, Binary raw, width, height, Binary small, w', h', scale)
  return (fromOnly ident)

--
-- for Edit.hs
--
insertImage :: HasPostgres m => Int -> L.ByteString -> Color -> [Circle] -> L.ByteString -> S.ByteString -> m Int
insertImage orig dat color circles delPass delSalt = do
  ident <- getByUnique $
           (query "INSERT INTO images(original, data, color, circles, delete_key, delete_key_salt) VALUES (?,?,?,?,?,?) RETURNING id")
           (orig, Binary dat, color, V.fromList circles, Binary delPass, Binary delSalt)
  return (fromOnly ident)

getImageFromRawImagesById :: (HasPostgres m, MonadSnap m) => Bool -> Int -> m (S.ByteString, Double)
getImageFromRawImagesById preview ident = do
  (dat,scale) <- getByUnique $
                 if preview
                 then query "SELECT small, scale FROM raw_images WHERE id = ?;" (Only ident)
                 else query "SELECT data,  scale FROM raw_images WHERE id = ?;" (Only ident)
  return (fromBinary dat, scale)

--
-- for Image.hs
--
selectOriginalFromImagesById :: (HasPostgres m, MonadSnap m) => Int -> m Int
selectOriginalFromImagesById ident =
  fromOnly `liftM`
  getByUnique (query "SELECT original FROM images WHERE id = ?;" (Only ident))

selectSmallSizeFromRawImagesById :: (HasPostgres m, MonadSnap m) => Int -> m (Int,Int)
selectSmallSizeFromRawImagesById orig =
  getByUnique (query "SELECT small_width, small_height FROM raw_images WHERE id = ?;" (Only orig))

selectImageFromImagesById :: (HasPostgres m, MonadSnap m) => Int -> m L.ByteString
selectImageFromImagesById ident =
  (fromBinary . fromOnly) `liftM`
  getByUnique (query "SELECT data FROM images WHERE id = ?;" (Only ident))

selectDeleteKeyById :: (HasPostgres m, MonadSnap m) => Int -> m (S.ByteString, S.ByteString)
selectDeleteKeyById ident =
  (\(k,s) -> (fromBinary k, fromBinary s)) `liftM`
  getByUnique (query "SELECT delete_key, delete_key_salt FROM images WHERE id = ?" (Only ident))

deleteImageById :: (HasPostgres m, MonadSnap m) => Int -> m Int64
deleteImageById ident =
  (execute "DELETE FROM images WHERE id = ?" (Only ident))







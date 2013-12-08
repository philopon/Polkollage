{-#LANGUAGE ForeignFunctionInterface#-}

module Cairo(surfaceWriteToPNGData, withImageSurfaceFromPNGData) where

import Control.Applicative
import Control.Exception
import Graphics.Rendering.Cairo
import Foreign.Ptr
import Foreign.ForeignPtr hiding (unsafeForeignPtrToPtr)
import Foreign.C
import Unsafe.Coerce
import Foreign.Marshal
import Data.IORef
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

--------------------------------------------------------------------------------

type StreamFunc a = Ptr a -> Ptr CUChar -> CUInt -> IO CInt

foreign import ccall "cairo_surface_write_to_png_stream" c'cairo_surface_write_to_png_stream :: Ptr Surface -> FunPtr (StreamFunc a) -> Ptr a -> IO CInt

foreign import ccall "wrapper" wrapStreamFunc :: StreamFunc a -> IO (FunPtr (StreamFunc a))

unSurfaceNewtype :: Surface -> ForeignPtr Surface
unSurfaceNewtype = unsafeCoerce

surfaceNewtype :: ForeignPtr Surface -> Surface
surfaceNewtype = unsafeCoerce

--------------------------------------------------------------------------------

surfaceWriteToPNGData :: Surface -> IO L.ByteString
surfaceWriteToPNGData surf = withForeignPtr (unSurfaceNewtype surf) $ \ptr -> do
  ref <- newIORef []
  fun <- wrapStreamFunc (writeFunc ref)
  r   <- c'cairo_surface_write_to_png_stream ptr fun nullPtr
  freeHaskellFunPtr fun
  if r == 0
    then L.fromChunks . reverse <$> readIORef ref
    else fail "PNGData write error."

writeFunc :: IORef [S.ByteString] -> StreamFunc a
writeFunc ioref _ inp len = do
  cstr <- S.packCStringLen (castPtr inp, fromIntegral len)
  modifyIORef ioref (cstr:)
  return 0

--------------------------------------------------------------------------------
foreign import ccall "cairo_image_surface_create_from_png_stream" c'cairo_image_surface_create_from_png_stream :: FunPtr (StreamFunc a) -> Ptr a -> IO (Ptr Surface)

foreign import ccall "cairo_surface_destroy" c'cairo_surface_destroy :: Ptr Surface -> IO ()

withImageSurfaceFromPNGData :: S.ByteString -> (Surface -> IO a) -> IO a
withImageSurfaceFromPNGData bs f =
  bracket
  (imageSurfaceFromPNGData bs)
  (\surf -> do
      withForeignPtr     (unSurfaceNewtype surf) c'cairo_surface_destroy
      finalizeForeignPtr (unSurfaceNewtype surf)
  ) f

imageSurfaceFromPNGData :: S.ByteString -> IO Surface
imageSurfaceFromPNGData bs = S.useAsCString bs $ \cstr -> do
  pos  <- newIORef 0
  fun  <- wrapStreamFunc (readFunc pos)
  surf <- c'cairo_image_surface_create_from_png_stream fun cstr
  freeHaskellFunPtr fun
  if surf /= nullPtr
    then surfaceNewtype <$> newForeignPtr_ surf
    else fail "PNGData read error."

readFunc :: IORef Int -> StreamFunc CChar
readFunc ref str red len = do
  pos <- readIORef ref
  copyBytes red (castPtr (plusPtr str pos)) (fromIntegral len)
  modifyIORef ref (+fromIntegral len)
  return 0

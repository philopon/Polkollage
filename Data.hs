{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, PackageImports, TupleSections, TemplateHaskell #-}

module Data( Circle(..)
           , left, top, radius, angle, scaleX, scaleY
           , Color(..) ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens








import qualified Data.Aeson as JSON
import           Data.Aeson.TH
------------------------------------------------------------------------------



import qualified Data.Vector as V
import           Data.Attoparsec.Number
import           Database.PostgreSQL.Simple.ToField

import           Blaze.ByteString.Builder.Char.Utf8
import Data.Monoid



data Circle = Circle { _left   :: Double
                     , _top    :: Double
                     , _radius :: Double
                     , _angle  :: Double
                     , _scaleX :: Double
                     , _scaleY :: Double
                     } deriving Show

instance ToField Circle where
  toField (Circle l t r a sx sy) = Plain $ fromChar '('
                                   <> fromShow  l <> fromChar ','
                                   <> fromShow  t <> fromChar ','
                                   <> fromShow  r <> fromChar ','
                                   <> fromShow  a <> fromChar ','
                                   <> fromShow sx <> fromChar ','
                                   <> fromShow sy <> fromString ")::arc"


deriveJSON defaultOptions{fieldLabelModifier = tail} ''Circle
makeLenses ''Circle

data Color = Color { red   :: Double
                   , green :: Double
                   , blue  :: Double
                   , alpha :: Double
                   } deriving Show

instance ToField Color where
  toField (Color r g b a) = Plain $ fromChar '('
                            <> fromShow r <> fromChar ','
                            <> fromShow g <> fromChar ','
                            <> fromShow b <> fromChar ','
                            <> fromShow a <> fromString ")::color"

getDouble :: Monad m => JSON.Value -> m Double
getDouble (JSON.Number (D d)) = return d
getDouble (JSON.Number (I i)) = return (fromIntegral i)
getDouble e                   = fail $ "cannot parse Double (" ++ show e ++ ")"

instance JSON.FromJSON Color where
  parseJSON (JSON.Array v) | V.length v >= 3 =
    Color <$> (getDouble (v V.! 0)) <*> (getDouble (v V.! 1)) <*> (getDouble (v V.! 2)) 
          <*> maybe (return 1) getDouble (v V.!? 3)

  parseJSON e = fail $ "cannt parse Color (" ++ show e ++ ")"

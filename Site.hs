{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, PackageImports, TupleSections #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import           Data.Monoid
import qualified Data.Text.Encoding as T
------------------------------------------------------------------------------
import           Application
import           Text.XmlHtml(Node(..))
import qualified Handler.Raw   as Raw
import qualified Handler.Edit  as Edit
import qualified Handler.Image as Image

getIndex :: AppHandler ()
getIndex = with sess $ do
  token <- csrfToken
  setInSession "token" token
  commitSession
  renderWithSplices "index" ("token" ## return [TextNode token])

getEdit :: AppHandler ()
getEdit = do
  Just ident <- getParam "id"
  renderWithSplices "edit" ("ident" ## return [TextNode $ T.decodeUtf8 ident])

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("",          serveDirectory "static")
         , ("",          method GET getIndex)
         , ("/edit/:id", method GET getEdit)
         ]

------------------------------------------------------------------------------
-- | The application initializer.
-- 

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    addConfig h $ mempty { hcLoadTimeSplices = "siteTitle" ## return [TextNode "Polkollage"] }
    d <- nestSnaplet "db" db pgsInit
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    addRoutes routes
    Raw.install    "raw"
    Edit.install   "edit"
    Image.install  "image"
    return $ App h s d


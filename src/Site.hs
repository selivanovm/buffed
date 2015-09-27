{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative()
import           Data.ByteString (ByteString)
import           Snap.Core()
import           Snap.Snaplet
import           Snap.Snaplet.Heist(heistInit)
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe

import           IndexHandler(handleIndex)
import           ApiHandler(handlePublicList, handleFeed, handleStartFetching, handleDownloadPost)

------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/app",               handleIndex)
         , ("/api/publicList",    handlePublicList)
         , ("/api/feed",          handleFeed)
         , ("/api/fetch",         handleStartFetching)
         , ("/api/download-post", handleDownloadPost)
         , ("",                   serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    addRoutes routes
    return $ App h s

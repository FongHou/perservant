module Api (mkApp) where

import Api.User
import Config (AppT (..), Config (..))
import Control.Exception.Safe (MonadThrow)
import Imports
import Servant
-- import Servant.Auth as SA
import Servant.Auth.Server as SAS

-- | Since we also want to provide a minimal front end, we need to give
-- Servant a way to serve a directory with HTML and JavaScript. This
-- function creates a WAI application that just serves the files out of the
-- given directory.
files :: Server Raw
files = serveDirectoryFileServer "assets"

-- | Just like a normal API type, we can use the ':<|>' combinator to unify
-- two different APIs and applications. This is a powerful tool for code
-- reuse and abstraction! We need to put the 'Raw' endpoint last, since it
-- always succeeds.
type AppAPI = SecureAPI :<|> Raw

type SecureAPI = UserAPI

-- type SecureAPI = Auth '[JWT] () :> UserAPI
-- instance FromJWT ()
-- instance ToJWT ()

apiServer :: (MonadIO m, MonadThrow m) => ServerT SecureAPI (AppT m)
apiServer = userServer

-- apiServer (Authenticated _) = userServer
-- apiServer _ = throwAll err401

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function.
toServer :: Config -> Server SecureAPI
toServer cfg = do
  hoistServerWithContext
    (Proxy :: Proxy SecureAPI)
    (Proxy :: Proxy '[SAS.CookieSettings, SAS.JWTSettings])
    (toHandler cfg)
    apiServer

-- | This function converts our @'AppT' m@ monad into the @ExceptT ServantErr
-- m@ monad that Servant's 'enter' function needs in order to run the
-- application.
toHandler :: Config -> AppT IO a -> Handler a
toHandler cfg appt = Handler $ runReaderT (runApp appt) cfg

-- | Finally, this function takes a configuration and runs our 'UserAPI'
-- alongside the 'Raw' endpoint that serves all of our files.
mkApp :: Config -> IO Application
mkApp cfg = do
  myKey <- readKey "jwt.key"
  let jwt = defaultJWTSettings myKey
      ctx = jwt :. defaultCookieSettings :. EmptyContext
  return $
    serveWithContext
      (Proxy :: Proxy AppAPI)
      ctx
      (toServer cfg :<|> files)

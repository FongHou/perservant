module Api.User where

import Config (AppT (..))
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Logger (logDebugNS)
import Control.Monad.Metrics (increment, metricsCounters)
import qualified Control.Monad.Metrics as Metrics
import Data.Generics.Labels ()
import Database.Persist.Postgresql
import Imports
import Models
import qualified Models as Md
import Servant
import Servant.JS (vanillaJS, writeJSForAPI)
import qualified System.Metrics.Counter as Counter

data Company = Company {name :: String, owner :: Person}
  deriving (Show, Generic)

data Person = Person {name :: String, age :: Int}
  deriving (Show, Generic)

display :: Company -> String
display c = c.name ++ " is run by " ++ c ^. #owner . #name

nameAfterOwner :: Company -> Company
nameAfterOwner c = c{name = c.owner.name ++ "'s Company"}

type UserAPI =
  "users" :> Get '[JSON] [Entity User]
    :<|> "users" :> Capture "name" Text :> Get '[JSON] (Entity User)
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
    :<|> "metrics" :> Get '[JSON] (HashMap Text Int64)

userApi :: Proxy UserAPI
userApi = Proxy

-- | The server that runs the UserAPI
userServer :: (MonadIO m, MonadThrow m) => ServerT UserAPI (AppT m)
userServer = allUsers :<|> singleUser :<|> createUser :<|> waiMetrics

-- | Returns all users in the database.
allUsers :: MonadIO m => AppT m [Entity User]
allUsers = do
  increment "allUsers"
  logDebugNS "web" "allUsers"
  runDb (selectList [] [])

-- | Returns a user by name or throws a 404 error.
singleUser :: (MonadIO m, MonadThrow m) => Text -> AppT m (Entity User)
singleUser str = do
  increment "singleUser"
  logDebugNS "web" "singleUser"
  maybeUser <- runDb (selectFirst [Md.UserName ==. str] [])
  case maybeUser of
    Nothing -> throwM err404
    Just person -> return person

-- | Creates a user in the database.
createUser :: MonadIO m => User -> AppT m Int64
createUser p = do
  increment "createUser"
  logDebugNS "web" "creating a user"
  newUser <- runDb (insert (User (userName p) (userEmail p)))
  return $ fromSqlKey newUser

-- | Return wai metrics as JSON
waiMetrics :: MonadIO m => AppT m (HashMap Text Int64)
waiMetrics = do
  increment "metrics"
  logDebugNS "web" "metrics"
  metr <- Metrics.getMetrics
  liftIO $ mapM Counter.read =<< readIORef (metr ^. metricsCounters)

-- | Generates JavaScript to query the User API.
generateJavaScript :: IO ()
generateJavaScript =
  writeJSForAPI (Proxy :: Proxy UserAPI) vanillaJS "./assets/api.js"

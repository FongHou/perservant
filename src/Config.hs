{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config where

import Control.Concurrent (ThreadId)
import Control.Exception.Safe (throwIO)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (MonadError)
import Control.Monad.Logger (MonadLogger (..))
import Control.Monad.Metrics (Metrics, MonadMetrics, getMetrics)
import qualified Data.ByteString.Char8 as BS
import Data.Pool
import Database.Persist.Postgresql (ConnectionString, SqlBackend, createPostgresqlPool)
import Database.PostgreSQL.Simple as PG
import Imports
import Logger
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Servant.Server (ServerError)
import System.Environment (lookupEnv)
import System.IO.Error (userError)

-- | This type represents the effects we want to have for our application.
-- We wrap the standard Servant monad with 'ReaderT Config', which gives us
-- access to the application configuration using the 'MonadReader'
-- interface's 'ask' function.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.
newtype AppT m a = AppT
  {runApp :: ReaderT Config (ExceptT ServerError m) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Config,
      MonadError ServerError,
      MonadThrow,
      MonadIO
    )

type App = AppT IO

-- | The Config for our application is (for now) the 'Environment' we're
-- running in and a Persistent 'ConnectionPool'.
data Config = Config
  { configPool :: Pool SqlBackend,
    configPGPool :: Pool PG.Connection,
    configEnv :: Environment,
    configMetrics :: Metrics,
    configEkgServer :: ThreadId,
    configLogEnv :: LogEnv,
    configPort :: Port
  }

instance Monad m => MonadMetrics (AppT m) where
  getMetrics = asks Config.configMetrics

-- | Katip instance for @AppT m@
instance MonadIO m => Katip (AppT m) where
  getLogEnv = asks configLogEnv
  localLogEnv = error "not implemented"

-- | MonadLogger instance to use within @AppT m@
instance MonadIO m => MonadLogger (AppT m) where
  monadLoggerLog = adapt logMsg

-- | MonadLogger instance to use in @makePool@
instance MonadIO m => MonadLogger (KatipT m) where
  monadLoggerLog = adapt logMsg

-- | Right now, we're distinguishing between three environments. We could
-- also add a @Staging@ environment if we needed to.
data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)

-- | This returns a 'Middleware' based on the environment that we're in.
setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout

-- | Web request logger (currently unimplemented and unused). For inspiration
-- see ApacheLogger from wai-logger package.
katipLogger :: LogEnv -> Middleware
katipLogger env app req respond = runKatipT env $ do
  -- todo: log proper request data
  logMsg "web" InfoS "todo: received some request"
  liftIO $ app req respond

-- | This function creates a 'ConnectionPool' for the given environment.
-- For 'Development' and 'Test' environments, we use a stock and highly
-- insecure connection string. The 'Production' environment acquires the
-- information from environment variables that are set by the keter
-- deployment application.
makePool :: Environment -> LogEnv -> IO (Pool SqlBackend, Pool Connection)
makePool Test env = do
  pool1 <- runKatipT env (createPostgresqlPool (connStr "") (envPool Test))
  pool2 <- createPool (PG.connectPostgreSQL (connStr "")) PG.close (envPool Test) 60000 1
  return (pool1, pool2)
makePool Development env = do
  pool1 <- runKatipT env $ createPostgresqlPool (connStr "") (envPool Development)
  pool2 <- createPool (PG.connectPostgreSQL (connStr "")) PG.close (envPool Development) 60000 1
  return (pool1, pool2)
makePool Production env = do
  -- This function makes heavy use of the 'MaybeT' monad transformer, which
  -- might be confusing if you're not familiar with it. It allows us to
  -- combine the effects from 'IO' and the effect of 'Maybe' into a single
  -- "big effect", so that when we bind out of @MaybeT IO a@, we get an
  -- @a@. If we just had @IO (Maybe a)@, then binding out of the IO would
  -- give us a @Maybe a@, which would make the code quite a bit more
  -- verbose.
  connStr <- runMaybeT $ do
    let keys = ["host=", "port=", "user=", "password=", "dbname="]
        envs = ["PGHOST", "PGPORT", "PGUSER", "PGPASS", "PGDATABASE"]
    envVars <- traverse (MaybeT . lookupEnv) envs
    pure $ BS.intercalate " " . zipWith (<>) keys $ BS.pack <$> envVars
  case connStr of
    -- If we don't have a correct database configuration, we can't
    -- handle that in the program, so we throw an IO exception. This is
    -- one example where using an exception is preferable to 'Maybe' or
    -- 'Either'.
    Nothing ->
      throwIO (userError "Database Configuration not present in environment.")
    Just connStr -> do
      pool1 <- runKatipT env $ createPostgresqlPool connStr (envPool Production)
      pool2 <- createPool (PG.connectPostgreSQL connStr) PG.close (envPool Production) 60000 1
      return (pool1, pool2)

-- | The number of pools to use for a given environment.
envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8

-- | A basic 'ConnectionString' for local/test development. Pass in either
-- @""@ for 'Development' or @"test"@ for 'Test'.
connStr :: BS.ByteString -> ConnectionString
connStr sfx =
  "host=localhost dbname=dvdrental"
    <> sfx
    <> " user=test password=testsecret port=5432"

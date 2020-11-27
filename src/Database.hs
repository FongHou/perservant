{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Database where

import Config
import Data.Fixed
import Data.Pool
import Data.Profunctor
import Data.Profunctor.Product
import Data.Profunctor.Product.Default (Default)
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Time
import Database.PostgreSQL.Simple as PG
import GHC.IO (unsafePerformIO)
import Imports
import Opaleye

type F field = Field field

type FNull field = FieldNullable field

mkUTCTime :: (Integer, Int, Int) -> (Int, Int, Pico) -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
  UTCTime
    (fromGregorian year mon day)
    (timeOfDayToTime (TimeOfDay hour min sec))

timestamps :: TableFields () (F SqlTimestamptz)
timestamps = readOnlyTableField "last_update"

withoutTS :: Table (a, ()) (b, c) -> Table a b
withoutTS tbl =
  tbl
    & lmap (,())
    & rmap fst

newtype ActorID = ActorID Int32
  deriving (Show, Eq)

newtype FilmID = FilmID Int32
  deriving (Show, Eq)

newtype CategoryID = CategoryID Int32
  deriving (Show, Eq)

instance DefaultFromField SqlInt4 ActorID where
  defaultFromField = ActorID <$> defaultFromField

data ActorT a b c = Actor
  { actorId :: a,
    firstName :: b,
    lastName :: c
  }

type Actor = ActorT ActorID Text Text

type ActorWriter = ActorT (Maybe (F SqlInt4)) (F SqlText) (F SqlText)

type ActorReader = ActorT (F SqlInt4) (F SqlText) (F SqlText)

deriving instance Show Actor

makeAdaptorAndInstanceInferrable "pActor" ''ActorT

actorTable ::
  Table
    (ActorWriter, ())
    (ActorReader, F SqlTimestamptz)
actorTable =
  table "actor" $
    p2
      ( pActor
          Actor
            { actorId = optionalTableField "actor_id",
              firstName = requiredTableField "first_name",
              lastName = requiredTableField "last_name"
            },
        timestamps
      )

actorNoTS :: Table ActorWriter ActorReader
actorNoTS = withoutTS actorTable

-- $> printSql selectActor

selectActor :: Select (ActorReader, F SqlTimestamptz)
selectActor = do
  (row@Actor {..}, lastUpdate) <- selectTable actorTable
  let time = mkUTCTime (2013, 1, 1) (0, 0, 0)
  viaLateral restrict (lastUpdate .>= sqlUTCTime time)
  pure (row, lastUpdate)

selectActorNoTS :: Select ActorReader
selectActorNoTS = selectTable actorNoTS

selectActorName :: Text -> Select (F SqlText, F SqlText)
selectActorName name = do
  Actor {..} <- selectActorNoTS
  viaLateral restrict (lastName .== sqlStrictText name)
  pure (firstName, lastName)

printSql :: Default Unpackspec a a => Select a -> IO ()
printSql = putStrLn . fromMaybe "Empty select" . showSql

conn :: Connection
conn = unsafePerformIO $ PG.connectPostgreSQL (connStr "")
{-# NOINLINE conn #-}

withDb :: (MonadReader Config m, MonadIO m) => (Connection -> IO b) -> m b
withDb query = do
  pool <- asks configPGPool
  liftIO $ withResource pool query

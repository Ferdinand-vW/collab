{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Database 
  (
    Connection,
    makeConnection,
    insertRoom,
    selectRoomById,
    selectRoomByName,
    selectUser,
    roomTable
  )
  where

import Database.PostgreSQL.Simple (Connection, ConnectInfo, connect, close, ConnectInfo(..))
import Opaleye
import qualified Opaleye.PGTypes as P
import qualified Opaleye.Constant as C
import Control.Arrow (returnA)
import Data.Profunctor.Product (p2, p3, p4)
import Data.Time.LocalTime (LocalTime)
import Data.Int (Int64)

----------------------TABLES-----------------------------------

roomTable :: Table (Maybe (Column PGInt8), Column PGText, Column PGTimestamp, (Column (Nullable PGInt8)))
                   (Column PGInt8, Column PGText, Column PGTimestamp, (Column (Nullable PGInt8)))
roomTable = Table "rooms" (p4 (optional "rid", required "rname", required "rcreated", required "uid"))

roomQuery :: Query (Column PGInt8, Column PGText, Column PGTimestamp, Column (Nullable PGInt8))
roomQuery = queryTable roomTable

userTable :: Table (Maybe (Column PGInt8), Column PGText)
                   (Column PGInt8, Column PGText)
userTable = Table "users" (p2 (optional "uid", required "uname"))


-------------QUERIES----------------------------

insertRoom :: Connection -> String -> LocalTime -> Maybe Integer -> IO [Integer]
insertRoom conn rname ltime muid = do
  runInsertManyReturning conn roomTable [(Nothing, pgString rname, pgLocalTime ltime, maybeToNullable $ fmap pgInteger8 muid)]
                                        (\(rid, _, _, _) -> rid)

selectRoomById' :: Integer -> Query (Column PGInt8, Column PGText, Column PGTimestamp, Column (Nullable PGInt8))
selectRoomById' n = proc () -> do
  row@(rid, _, _, _) <- roomQuery -< ()

  restrict -< rid .== pgInteger8 n

  returnA -< row

selectRoomById :: Connection -> Integer -> IO [(Integer, String, LocalTime, Maybe Integer)]
selectRoomById conn n = runQuery conn (selectRoomById' n)

selectRoomByName' :: String -> Query (Column PGInt8, Column PGText, Column PGTimestamp, Column (Nullable PGInt8))
selectRoomByName' s = proc () -> do
  row@(_, rname, _, _) <- roomQuery -< ()

  restrict -< rname .== pgString s

  returnA -< row

selectRoomByName :: Connection -> String -> IO [(Integer, String, LocalTime, Maybe Integer)]
selectRoomByName conn s = runQuery conn (selectRoomByName' s)

selectUser :: Connection -> String -> IO [(Integer, String)]
selectUser conn s = runQuery conn (queryTable userTable)


----------------DATABASE INFO -------------------
makeConnection :: IO Connection
makeConnection = connect connInfo


connInfo :: ConnectInfo
connInfo = ConnectInfo
  { connectHost = "127.0.0.1"
  , connectPort = 5432
  , connectUser = "Admin"
  , connectPassword = ""
  , connectDatabase = "CollabDB"
  }

pgInteger8 :: Integer -> Column PGInt8
pgInteger8 n = pgInt8 $ fromIntegral n 

instance QueryRunnerColumnDefault PGInt8 Integer where
  queryRunnerColumnDefault = fieldQueryRunnerColumn
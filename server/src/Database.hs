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
    selectUserByName,
    insertUser,
    selectUserPassword,
    roomTable
  )
  where

import Database.PostgreSQL.Simple (Connection, ConnectInfo, connect, close, ConnectInfo(..))
import Opaleye
import qualified Opaleye.PGTypes as P
import qualified Opaleye.Constant as C
import Control.Arrow (returnA)
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (listToMaybe)
import Data.Profunctor.Product (p2, p3, p4)
import Data.Time.LocalTime (LocalTime)
import Data.Int (Int64)

----------------------TABLES-----------------------------------

roomTable :: Table (Maybe (Column PGInt8), Column PGText, Column PGTimestamp, (Column (Nullable PGInt8)))
                   (Column PGInt8, Column PGText, Column PGTimestamp, (Column (Nullable PGInt8)))
roomTable = Table "rooms" (p4 (optional "rid", required "rname", required "rcreated", required "uid"))

roomQuery :: Query (Column PGInt8, Column PGText, Column PGTimestamp, Column (Nullable PGInt8))
roomQuery = queryTable roomTable

userTable :: Table (Maybe (Column PGInt8), Column PGText, Column PGBytea)
                   (Column PGInt8, Column PGText, Column PGBytea)
userTable = Table "users" (p3 (optional "uid", required "uname", required "upassword"))

userQuery :: Query (Column PGInt8, Column PGText, Column PGBytea)
userQuery = queryTable userTable


-------------QUERIES----------------------------


-- >>>>>>>> Room queries >>>>>>>>>>>>>
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

-- >>>>>>>> User queries >>>>>>>>>>>>>

insertUser :: Connection -> String -> ByteString -> IO [Integer]
insertUser conn uname upass = runInsertManyReturning conn userTable [(Nothing, pgString uname, pgStrictByteString upass)] (\(uid, _, _) -> uid) 

selectUserByName' :: String -> Query (Column PGInt8, Column PGText, Column PGBytea)
selectUserByName' s = proc () -> do
  row@(_, uname, _) <- userQuery -< ()

  restrict -< uname .== pgString s

  returnA -< row

selectUserByName :: Connection -> String -> IO [(Integer, String, ByteString)]
selectUserByName conn s = runQuery conn (selectUserByName' s)

selectUserPassword' :: String -> Query (Column PGBytea)
selectUserPassword' s = proc () -> do
  row@(_, uname, upass) <- userQuery -< ()

  restrict -< uname .== pgString s

  returnA -< upass

selectUserPassword :: Connection -> String -> IO (Maybe ByteString)
selectUserPassword conn s = fmap listToMaybe $ runQuery conn (selectUserPassword' s)


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
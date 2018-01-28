{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module App where

import Data.ByteString.Char8 as B
import Data.Default (def)
import Data.List (find)
import Data.Time.Clock
import Control.Monad
import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Crypto.Random (drgNew)
import Servant
import Servant.Utils.Enter (enter, (:~>)(NT),)
import Servant.Server.Experimental.Auth.Cookie

import Database
import Object.Room
import Object.User
import Time

newtype AppM m a = AppM { runAppM :: ReaderT Config (LoggingT m) a }

data Config = Config { writeLock :: MVar ()
                     , conn :: Connection
                     , cookies :: AuthCookieSettings
                     , rndSource :: RandomSource
                     , serverKey :: PersistentServerKey
                     }

newConfig :: IO Config
newConfig = do
  conn <- makeConnection
  mv <- newMVar ()
  let settings = (def :: AuthCookieSettings) {acsCookieFlags = [B.pack "HttpOnly"]}
  rs <- mkRandomSource drgNew 1000
  let sk = mkPersistentServerKey (B.pack "0123456789abcdef")
  return $ Config mv conn settings rs sk

instance Monad m => Functor (AppM m) where
  fmap f (AppM r) = AppM $ fmap f r

instance Monad m => Applicative (AppM m) where
  pure x = return x
  (<*>) = ap

instance Monad m => Monad (AppM m) where
  return x = AppM $ return x
  (AppM r) >>= f = AppM $ r >>= runAppM . f

instance (Monad m, MonadIO m) => MonadIO (AppM m) where
  liftIO io = AppM $ lift $ liftIO io

instance MonadTrans AppM where
  lift m = AppM $ lift $ lift m

instance MonadIO m => MonadLogger (AppM m) where
  monadLoggerLog a b c d = AppM $ monadLoggerLog a b c d

instance MonadIO m => MonadLoggerIO (AppM m) where
  askLoggerIO = AppM askLoggerIO

getConn :: Monad m => AppM m Connection
getConn = AppM $ do
  config <- ask
  return $ conn config

getConfig :: Monad m => AppM m Config
getConfig = AppM ask

getWriteLock :: Monad m => AppM m (MVar ())
getWriteLock = AppM $ do
  config <- ask
  return $ writeLock config

sItem :: (MonadIO m, Monad m) => IO [a] -> AppM m a
sItem io = do
  d <- liftIO io
  case d of
    [] -> fail "Could not retrieve result from database"
    (x:_) -> return x

mItem :: (MonadIO m, Monad m) => IO [a] -> AppM m (Maybe a)
mItem io = do
  d <- liftIO io
  case d of
    [] -> return Nothing
    (x:_) -> return $ Just x

appToHandle' :: Config -> AppM Handler a -> Handler a
appToHandle' cfg app = do
  let logger = (runReaderT (runAppM app) cfg)
  runStdoutLoggingT logger

appToHandle :: Config -> AppM Handler :~> Handler
appToHandle cfg = NT $ appToHandle' cfg

addUser :: MonadIO m => String -> ByteString -> AppM m Integer
addUser uname upass = do
  conn <- getConn
  sItem $ insertUser conn uname upass

getUserByName :: MonadIO m => String -> AppM m (Maybe User)
getUserByName s = do
  conn <- getConn
  utuple <- mItem $ selectUserByName conn s
  return $ fmap (\(uid, uname, upass) -> User uid uname upass) utuple

getRoomById :: MonadIO m => Integer -> AppM m (Maybe Room)
getRoomById n = do
  conn <- getConn
  rtuple <- mItem $ selectRoomById conn n
  return $ fmap (\(rid,rname, ltime,uid) -> Room rid rname (toDateTime ltime) uid) rtuple

getRoomByName :: MonadIO m => String -> AppM m (Maybe Room)
getRoomByName s = do
  conn <- getConn
  rtuple <- mItem $ selectRoomByName conn s
  return $ fmap (\(rid,rname, ltime,uid) -> Room rid rname (toDateTime ltime) uid) rtuple

makeRoomWUser :: MonadIO m => String -> String -> AppM m Integer
makeRoomWUser rname uname = do
  ltime <- liftIO $ getLocalTime
  conn <- getConn
  muser <- getUserByName uname
  sItem $ insertRoom conn rname ltime (fmap uid muser)

makeRoom :: MonadIO m => String -> AppM m Integer
makeRoom rname = do
  ltime <- liftIO $ getLocalTime
  conn <- getConn
  sItem $ insertRoom conn rname ltime Nothing

      
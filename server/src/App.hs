{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module App where

import Data.List (find)
import Data.Time.Clock
import Control.Monad
import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Servant
import Servant.Utils.Enter (enter, (:~>)(NT),)

import Database
import API (api)
import Room
import Time

newtype AppM m a = AppM { runAppM :: ReaderT Config (LoggingT m) a }

data Config = Config { writeLock :: MVar ()
                     , conn :: Connection }

newConfig :: IO Config
newConfig = do
  conn <- makeConnection
  mv <- newMVar ()
  return $ Config mv conn

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
  (uid, _) <- sItem $ selectUser conn uname
  sItem $ insertRoom conn rname ltime (Just uid)

makeRoom :: MonadIO m => String -> AppM m Integer
makeRoom rname = do
  ltime <- liftIO $ getLocalTime
  conn <- getConn
  sItem $ insertRoom conn rname ltime Nothing

      
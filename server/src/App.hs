{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module App where

import Data.List (find)
import Data.Time.Clock
import Control.Monad
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

newtype AppM m a = AppM { runAppM :: ReaderT Connection (LoggingT m) a }

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

appToHandle' :: AppM Handler a -> Handler a
appToHandle' app = do
  conn <- liftIO makeConnection
  let logger = (runReaderT (runAppM app) conn)
  runStdoutLoggingT logger

appToHandle :: AppM Handler :~> Handler
appToHandle = NT appToHandle'

getRoom :: MonadIO m => Integer -> AppM m (Maybe Room)
getRoom n = do
  conn <- AppM ask
  rtuple <- mItem $ selectRoom conn n
  return $ fmap (\(rid,ltime,uid) -> Room rid (toDateTime ltime) uid) rtuple

makeRoomWUser :: MonadIO m => String -> AppM m Integer
makeRoomWUser uname = do
  ltime <- liftIO $ getLocalTime
  conn <- AppM ask
  (uid, _) <- sItem $ selectUser conn uname
  sItem $ insertRoom conn ltime (Just uid)

makeRoom :: MonadIO m => AppM m Integer
makeRoom = do
  ltime <- liftIO $ getLocalTime
  conn <- AppM ask
  sItem $ insertRoom conn ltime Nothing

      
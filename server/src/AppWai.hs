module AppWai where

import Control.Concurrent
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Class

import Servant
import Servant.Server
import Servant.Server.Internal.ServantErr (responseServantErr)
import Network.Wai (ResponseReceived, Request, Response)

import App
import Database

newtype AppHWai a = AppHWai { runAppHWai :: Request -> (Response -> IO ResponseReceived) -> AppM Handler a }

instance Functor AppHWai where
  fmap f m = pure f <*> m

instance Applicative AppHWai where
  pure = return
  (<*>) = ap

instance Monad AppHWai where
  return a = AppHWai $ \_ _ -> return a
  (AppHWai f) >>= g = AppHWai $ \req rsp -> do
    a <- f req rsp
    runAppHWai (g a) req rsp

instance MonadIO AppHWai where
  liftIO m = AppHWai $ \_ _ -> liftIO m

liftH :: Handler a -> AppHWai a
liftH h = AppHWai $ \_ _ -> lift h

liftA :: AppM Handler a -> AppHWai a
liftA m = AppHWai $ \_ _ -> m

liftRaw :: Server Raw -> AppHWai ResponseReceived
liftRaw s = AppHWai $ \req f -> liftIO $ unTagged s req f

runWaiAsApp :: Config -> AppHWai ResponseReceived -> Application
runWaiAsApp cfg (AppHWai f) req resp = do
  eth <- runHandler $ runStdoutLoggingT $ runReaderT (runAppM $ f req resp) cfg
  case eth of
    Left err -> resp $ responseServantErr err
    Right rsp -> return rsp 

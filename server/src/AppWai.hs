module AppWai where

import Control.Concurrent
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Class

import Servant
import Servant.Server.Internal.ServantErr (responseServantErr)
import Network.Wai (ResponseReceived, Request, Response)

import App
import Database

type HandlerWai = Request -> (Response -> IO ResponseReceived) -> Handler ResponseReceived

handlerToWai :: HandlerWai -> Application
handlerToWai h req resp = do
  eth <- runHandler $ h req resp
  case eth of
    Left err -> resp $ responseServantErr err
    Right resp -> return resp


type AppMWai = Request -> (Response -> IO ResponseReceived) -> AppM Handler ResponseReceived

handlerToAppWai :: HandlerWai -> AppMWai
handlerToAppWai f req resp = lift $ f req resp

appToHandlerWai :: Config -> AppMWai -> HandlerWai
appToHandlerWai cfg f req resp = do
  runStdoutLoggingT $ runReaderT (runAppM $ f req resp) cfg

runWaiAsApp :: Config -> AppMWai -> Application
runWaiAsApp config = handlerToWai . appToHandlerWai config

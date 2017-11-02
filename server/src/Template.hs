module Template where


import           Control.Monad.IO.Class
import           Text.Mustache
import qualified Data.Text as T

compile :: ToMustache a => String -> a -> String -> IO ()
compile src a trg = do
    compiled <- liftIO $ automaticCompile ["./templates"] src

    case compiled of
        Left err -> liftIO $ print err
        Right tmp -> do
            let t = substitute tmp a
            liftIO $ writeFile trg (T.unpack t)

module Main where

import Server (app)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8081 app




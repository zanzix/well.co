module Main (main) where

import Server
import Network.Wai.Handler.Warp

main :: IO ()
main = run 9000 =<< runServer

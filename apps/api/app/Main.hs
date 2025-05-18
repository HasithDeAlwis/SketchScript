module Main where

import App.Server (app)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main =
  putStrLn "Running on Port 8081"
    >> app
    >>= run 8080

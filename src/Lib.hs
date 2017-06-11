{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( app
    ) where


import           Control.Concurrent  (threadDelay)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Data.Text           (Text)
import           Pipes
import           Pipes.Concurrent
import           Network.Socket      (withSocketsDo)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS


receiver :: WS.Connection -> Producer Text IO r
receiver conn = do
  lift $ T.putStrLn "Connected!"
  forever $ do
    msg <- lift $ WS.receiveData conn
    yield msg

printer :: Consumer Text IO ()
printer = do
  evt <- await
  lift $ T.putStr "MSG: "
  lift $ T.putStrLn evt
  printer

-- Should be forked
sendMSGs :: WS.ClientApp ()
sendMSGs conn = loop 0
  where
    loop n = do
      threadDelay 1000000
      WS.sendTextData conn $ T.pack $ show n
      loop (n + 1)

app :: IO ()
app = withSocketsDo $
  WS.runClient "echo.websocket.org" 80 "/" $ \conn -> do
    forkIO $ sendMSGs conn
    (output, input) <- spawn unbounded
    forkIO $ do
      runEffect $ receiver conn >-> toOutput output
      performGC
    runEffect $ fromInput input >-> printer

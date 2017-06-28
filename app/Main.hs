{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Except
import           Control.Monad.IO.Class (liftIO)
import           Data.Monoid            ((<>))
import           Data.Text
import           System.Environment

import           Network.Funky


bot :: String -> Client
bot t =
  Client
  { token    = pack t
  , prefix   = "!"
  , commands = [add, ping]
  , handlers = [handler]
  }

main :: IO ()
main = runClient . bot =<< getEnv "DISCORD_TOKEN1"

handler :: Handler ()
handler (MessageCreate m) = liftIO $ print $ msgContent m
handler _                 = liftIO $ putStrLn "foo"

ping :: DiscordCommand
ping =
  Command
  "ping"
  (const $ pure HNil)
  (do
    b <- say "pong"
    liftIO . print $ b
  `catchError` (liftIO . print) )
  (\_ _ -> say_ "An error occurred?")

add :: DiscordCommand
add =
  Command
  "add"
  (wargs $ int `RFCons` int `RFCons` RFNil)
  (\a b -> say_ $ "The answer is: " <> (pack . show $ a + b))
  (\_ _ -> say_ "Please give me 2 integers")

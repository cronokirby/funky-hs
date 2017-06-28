{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Data.Monoid ((<>))
import Data.Text

import Network.Funky


bot :: Client
bot =
  Client
  { token    = "MjY3MjM1NDYyMTIzNDg3MjMy.DCYMeQ.Lc6FLa80unMSzOw4T6GGFSBlgck"
  , prefix   = "!"
  , commands = [add, ping]
  , handlers = [handler]
  }

main :: IO ()
main = runClient bot

handler :: Handler ()
handler (MessageCreate m) = liftIO $ print $ msgContent m
handler _ = liftIO $ putStrLn "foo"

ping :: DiscordCommand
ping =
  Command
  "ping"
  (const $ pure HNil)
  (say_ "pong!")
  (\_ _ -> say_ "An error occurred?")

add :: DiscordCommand
add =
  Command
  "add"
  (wargs $ int `RFCons` int `RFCons` RFNil)
  (\a b -> say_ $ "The answer is: " <> (pack . show $ a + b))
  (\_ _ -> say_ "Please give me 2 integers")

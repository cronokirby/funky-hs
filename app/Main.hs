{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import Data.Text
import Control.Monad.IO.Class

import Network.Funky


bot :: Client
bot =
  Client
  { token    = "token"
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
  (say "pong!")
  (\_ _ -> say "An error occurred?")

add :: DiscordCommand
add =
  Command
  "add"
  (wargs $ int `RFCons` int `RFCons` RFNil)
  (\a b -> say $ "The answer is: " <> (pack . show $ a + b))
  (\_ _ -> say "Please give me 2 integers")
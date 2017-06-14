{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class

import Network.Funky

h :: Handler ()
h (MessageCreate m) = liftIO $ print $ msgContent m
h _ = liftIO $ putStrLn "foo"

c :: Client
c = Client "MjY3MjM1NDYyMTIzNDg3MjMy.DCMY3w.JHZ-3t9K3qfYnj494CBbO39WIiE" [h]

main :: IO ()
main = runClient c

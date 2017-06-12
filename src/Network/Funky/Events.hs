{-# LANGUAGE OverloadedStrings #-}
module Network.Funky.Events
    ( eventConsumer
    , Handler
    )
where

import           Data.Aeson         (FromJSON, Value)
import           Data.Aeson.Types   (parseMaybe, parseJSON)
import           Control.Concurrent (forkIO)
import           Control.Monad      (forever)
import           Pipes
import qualified Data.Text        as T
import qualified Pipes.Prelude    as P

import Network.Funky.Gateway
import Network.Funky.Types


decode :: FromJSON a => Value -> Maybe a
decode = parseMaybe parseJSON

data Event = MessageCreate Message | NoOp

mkEvent :: T.Text -> Value -> Maybe Event
mkEvent "MESSAGE_CREATE" o = MessageCreate <$> decode o
mkEvent _ _                = Nothing

filterMaybe :: Monad m => Pipe (Maybe a) a m ()
filterMaybe = forever $ do
  m <- await
  case m of
    Nothing -> discard m
    Just x  -> yield x

type Handler a = Event -> DiscordM a

runHandlers :: DiscordST -> [Handler a] -> Consumer Event IO ()
runHandlers st handlers = do
  evt <- await
  liftIO $ mapM_ (forkIO . void . (`runDiscordM` st) . ($ evt)) handlers

eventConsumer :: DiscordST -> [Handler a]
              -> Consumer (T.Text, Value) IO ()
eventConsumer st h =
  P.map (uncurry mkEvent) >-> filterMaybe >-> runHandlers st h

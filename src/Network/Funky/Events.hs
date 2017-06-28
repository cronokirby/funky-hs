{-# LANGUAGE OverloadedStrings #-}
module Network.Funky.Events
    ( eventConsumer
    , Handler
    , Event(..)
    , CommandM
    , runCommandM
    , liftDM
    )
where

import           Control.Concurrent     (forkIO)
import           Control.Monad          (forever)
import           Control.Monad.Reader
import           Data.Aeson             (FromJSON, Value)
import           Data.Aeson.Types       (parseJSON, parseMaybe)
import           Data.Char              (isSpace)
import           Data.HashMap.Strict    as HM
import qualified Data.Text              as T
import           Pipes
import qualified Pipes.Prelude          as P

import           Control.Funky.Commands
import           Network.Funky.Gateway
import           Network.Funky.Types


decode :: FromJSON a => Value -> Maybe a
decode = parseMaybe parseJSON

data Event = MessageCreate Message | NoOp deriving (Show)

mkEvent :: T.Text -> Value -> Maybe Event
mkEvent "MESSAGE_CREATE" o = MessageCreate <$> decode o
mkEvent _ _                = Just NoOp

filterMaybe :: Monad m => Pipe (Maybe a) a m ()
filterMaybe = forever $ do
    m <- await
    case m of
      Nothing -> discard m
      Just x  -> yield x


type Handler a = Event -> DiscordM a

runHandlers :: DiscordST -> [Handler a] -> Consumer Event IO ()
runHandlers st handlers = forever $ do
    evt <- await
    liftIO $ mapM_ (forkIO . void . (`runDiscordM` st) . ($ evt)) handlers

type CommandM = ReaderT Message DiscordM

type CommandMap = HashMap T.Text (Command (CommandM ()))

liftDM :: DiscordM a -> CommandM a
liftDM = lift

runCommandM :: CommandM a -> Message -> DiscordST -> IO (Either DiscordError a)
runCommandM = (runDiscordM .) . runReaderT

runCommands :: DiscordST -> T.Text -> CommandMap -> Pipe Event Event IO ()
runCommands st prefix cmds = forever $ do
    evt <- await
    liftIO $ handleEVT evt
    yield evt
  where
    handleEVT (MessageCreate msg) =
        maybe (liftIO $ putStrLn "no cmd") (runCMD msg) $ do
            t <- T.stripPrefix prefix (msgContent msg)
            let (first, rest) = T.break isSpace t
            cmd <- HM.lookup first cmds
            pure (cmd, rest)
    handleEVT _ = pure ()
    runCMD msg (cmd, rest) =
        void $ runCommandM (runCommand cmd rest) msg st

eventConsumer :: DiscordST -> [Handler a] -> T.Text
              -> CommandMap
              -> Consumer Yield IO ()
eventConsumer state handlers prefix commands =
    P.map (uncurry mkEvent)
    >-> filterMaybe
    >-> runCommands state prefix commands
    >-> runHandlers state handlers


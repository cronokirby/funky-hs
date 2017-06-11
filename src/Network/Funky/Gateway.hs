{-# LANGUAGE OverloadedStrings #-}
module Network.Funky.Gateway
    (

    )
where

import           Codec.Compression.Zlib (decompress)
import           Control.Applicative (liftA2, (<|>))
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens ((^?), (^..))
import           Control.Monad (forever)
import           Control.Monad.Trans.State hiding (State)
import           Data.Aeson
import           Data.Aeson.Types (parseMaybe, Parser)
import           Data.ByteString (ByteString)
import           Data.Maybe (fromMaybe)
import           Data.Semigroup ((<>))
import           Pipes
import           Pipes.Concurrent
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Network.WebSockets as WS


data State =
  State
  { token :: ByteString
  , tSeq :: TVar Int
  , trace :: [T.Text]
  , sessionID :: Maybe T.Text
  , conn :: WS.Connection -- Useful here
  }

mkState :: ByteString -> WS.Connection -> IO State
mkState token conn = do
  tvar <- atomically $ newTVar 0
  return $ State token tvar [] Nothing conn


data PayD
    = PayInt Int
    | PayBool Bool
    | PayVal Value

data Payload = Payload Int (Maybe (Int, T.Text)) PayD

instance FromJSON Payload where
  parseJSON (Object o) = Payload
    <$> o .: "op"
    <*> (liftA2 (,) <$> o .:? "s" <*> o .:? "t")
    <*> (PayInt  <$> o .: "d" <|>
         PayBool <$> o .: "d" <|>
         PayVal  <$> o .: "d")


newtype OutLoad
    = HeartBeat Int

instance ToJSON OutLoad where
  toJSON (HeartBeat i) = mkOP 1 $ toJSON i
    where
      mkOP :: Int -> Value -> Value
      mkOP i v = object ["op" .= i, "d" .= v]


gateway :: ByteString ->
           WS.Connection -> Producer (T.Text, ByteString) IO r
gateway token conn = do
  state <- liftIO $ mkState token conn
  gatewayLoop state

gatewayLoop :: State -> Producer (T.Text, ByteString) IO r
gatewayLoop st = do
  msg <- liftIO $ WS.receiveDataMessage (conn st)
  st <- liftIO $ execStateT (dispatch $ mkPayload msg) st
  gatewayLoop st


mkPayload :: WS.DataMessage -> Maybe Payload
mkPayload (WS.Text b )  = decode b
mkPayload (WS.Binary b) = decode $ decompress b


dispatch :: Maybe Payload -> StateT State IO ()

dispatch (Just (Payload 1 _ _)) = do
  tvar <- gets tSeq
  sq <- liftIO $ readTVarIO tvar
  sendJSON $ HeartBeat sq

dispatch (Just (Payload 9 _ _)) =
  liftWS WS.sendClose ("closing" :: ByteString)

dispatch (Just (Payload 10 _ (PayVal o))) =
  fromMaybe (pure ()) $
  liftA2 startBeating
  (getObj o (.: "_traces"))
  (getObj o (.: "heartbeat_interval"))

dispatch _ =
  return ()


startBeating :: [T.Text] -> Integer -> StateT State IO ()
startBeating trce int = do
  tvar <- gets tSeq
  liftWS (beater $ fromIntegral int) tvar
  modify (\s -> s {trace = trce})

-- This will die if the conn dies, after throwing an exception
beater :: Int -> WS.Connection -> TVar Int -> IO ()
beater interval conn tseq = void . forkIO . forever $ do
  sq <- readTVarIO tseq
  WS.sendTextData conn $ encode $ HeartBeat sq
  threadDelay $ interval * 1000

-- Utility --

liftWS :: (WS.Connection -> a -> IO ()) -> a -> StateT State IO ()
liftWS f a = gets conn >>= liftIO . flip f a

-- Should be used in the dispatch function only
sendJSON :: ToJSON a => a -> StateT State IO ()
sendJSON = liftWS WS.sendTextData . encode

getObj :: Value -> (Object -> Parser a) -> Maybe a
getObj (Object o) f = parseMaybe f o
getObj _ _          = Nothing

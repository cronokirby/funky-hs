{-# LANGUAGE OverloadedStrings #-}
module Network.Funky.Gateway
    ( Yield
    , gateway
    , runGateway
    )
where

import           Codec.Compression.Zlib    (decompress)
import           Control.Applicative       (liftA2, (<|>))
import           Control.Concurrent        hiding (yield)
import           Control.Concurrent.STM
import           Control.Lens              ((^.))
import           Control.Monad             (forever)
import           Control.Monad.Trans.State hiding (State)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types          (Parser, parseMaybe)
import           Data.Maybe                (fromMaybe)
import qualified Data.Text                 as T
import qualified Network.WebSockets        as WS
import           Network.Wreq              as W hiding (Payload)
import           Pipes
import           System.Info               (os)
import           Wuss


data State = State
    { token     :: !T.Text
    , tSeq      :: TVar Int
    , trace     :: ![T.Text]
    , sessionID :: Maybe T.Text
    , shard     :: (Int, Int)
    , conn      :: !WS.Connection -- Useful here
    }

mkState :: T.Text -> WS.Connection -> IO State
mkState token conn = do
    tvar <- atomically (newTVar 0)
    return $ State token tvar [] Nothing (0, 1) conn


data PayD
    = PayInt Int
    | PayBool Bool
    | PayVal Value

data Payload = Payload Int (Maybe (T.Text, Int)) PayD

instance FromJSON Payload where
    parseJSON (Object o) = Payload
        <$> o .: "op"
        <*> (liftA2 (,) <$> o .:? "t" <*> o .:? "s")
        <*> (PayInt  <$> o .: "d" <|>
             PayBool <$> o .: "d" <|>
             PayVal  <$> o .: "d")


data OutLoad
    = HeartBeat Int
    | Identify T.Text (Int, Int)

instance ToJSON OutLoad where
    toJSON (HeartBeat i)             = mkOP 1 $ toJSON i
    toJSON (Identify token (s1, s2)) = mkOP 2 $
        object [ "token"           .= token
               , "properties"      .= properties os
               , "compress"        .= True
               , "large_threshold" .= (250 :: Int)
               , "shard"           .= [s1, s2]
               ]
      where
        properties os =
            object [ "os"               .= os
                   , "browser"          .= ("funky" :: T.Text)
                   , "device"           .= ("funky" :: T.Text)
                   , "referrer"         .= ("" :: T.Text)
                   , "referring_domain" .= ("" :: T.Text)
                   ]

mkOP :: Int -> Value -> Value
mkOP i v = object ["op" .= i, "d" .= v]


type Yield = (T.Text, Value)

gateway :: T.Text -> WS.Connection -> Producer Yield IO r
gateway token conn = do
    st <- liftIO $ mkState token conn
    gatewayLoop st

gatewayLoop :: State -> Producer Yield IO r
gatewayLoop st = do
    msg        <- liftIO $ WS.receiveDataMessage (conn st)
    (a, newSt) <- liftIO $ runStateT (dispatch $ mkPayload msg) st
    maybe (pure ()) yield a
    gatewayLoop newSt


mkPayload :: WS.DataMessage -> Maybe Payload
mkPayload (WS.Text b )  = decode b
mkPayload (WS.Binary b) = decode $ decompress b

type GatewayM = StateT State IO

dispatch :: Maybe Payload -> GatewayM (Maybe Yield)

dispatch (Just (Payload 1 _ _)) = do
    liftIO $ putStrLn "payload 1"
    tvar <- gets tSeq
    sq   <- liftIO $ readTVarIO tvar
    sendJSON $ HeartBeat sq
    return Nothing

dispatch (Just (Payload 9 _ _)) = do
    liftWS WS.sendClose ("closing" :: T.Text)
    return Nothing

dispatch (Just (Payload 10 _ (PayVal o))) =
    fromMaybe (pure ())
    (liftA2 startBeating
    (getObj o (.: "_trace"))
    (getObj o (.: "heartbeat_interval"))) *> do
        identify
        pure Nothing

dispatch (Just (Payload _ (Just ("READY", sq)) (PayVal o))) = do
    updateSeq sq
    update
    pure Nothing
  where
    update = fromMaybe (pure ()) $ do
        session <- getObj o (.: "session_id")
        trc     <- getObj o (.: "_trace")
        pure $ modify (\s -> s {sessionID = session, trace = trc})

dispatch (Just (Payload _ (Just ("RESUMED", sq)) (PayVal o))) =
    pure Nothing <*
    fromMaybe (pure ())
    (update <$> getObj o (.: "_trace"))
  where
    update t = modify (\s -> s {trace = t})

dispatch (Just (Payload _ (Just (t, sq)) (PayVal o))) = do
    updateSeq sq
    pure $ Just (t, o)

dispatch _ =
    pure Nothing


updateSeq :: Int -> GatewayM ()
updateSeq sq = do
    tvar <- gets tSeq
    liftIO . atomically $ writeTVar tvar sq

identify :: GatewayM ()
identify = do
    session <- gets sessionID
    maybe sendIdentify resume session
  where
    resume _ = pure ()
    sendIdentify = do
        tok  <- gets token
        shrd <- gets shard
        sendJSON $ Identify tok shrd


startBeating :: [T.Text] -> Integer -> GatewayM ()
startBeating trce int = do
    tvar <- gets tSeq
    liftWS (beater $ fromIntegral int) tvar
    modify (\s -> s {trace = trce})

-- This will die if the conn dies, after throwing an exception
beater :: Int -> WS.Connection -> TVar Int -> IO ()
beater interval conn tseq = void . forkIO . forever $ do
    sq <- readTVarIO tseq
    WS.sendTextData conn . encode $ HeartBeat sq
    threadDelay (interval * 1000)

-- Running the gateway --

getURL :: IO T.Text
getURL = do
    r <- W.get "https://discordapp.com/api/v6/gateway"
    return $ r ^. responseBody ^. key "url" . _String

runGateway :: WS.ClientApp () -> IO ()
runGateway ws = do
    url <- getURL
    runSecureClient "gateway.discord.gg" 443 "/?v=6&encoding=json" ws

-- Utility --

liftWS :: (WS.Connection -> a -> IO ()) -> a -> GatewayM ()
liftWS f a = gets conn >>= liftIO . flip f a

-- Should be used in the dispatch function only
sendJSON :: ToJSON a => a -> GatewayM ()
sendJSON = liftWS WS.sendTextData . encode

getObj :: Value -> (Object -> Parser a) -> Maybe a
getObj (Object o) f = parseMaybe f o
getObj _ _          = Nothing

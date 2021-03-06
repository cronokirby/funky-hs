{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Network.Funky.Types
    ( module Network.Funky.Types.Base
    , module Network.Funky.Types.Channel
    , module Network.Funky.Types.Message
    , module Network.Funky.Types.User
    , DiscordError(..)
    , DiscordM
    , DiscordST(..)
    , mkDiscordST
    , runDiscordM
    , testDiscordM
    )
where


import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.ByteString             (ByteString)
import           Data.HashMap.Strict
import           Data.Text                   (Text)
import           Network.HTTP.Req            (HttpException, MonadHttp,
                                              handleHttpException)

import           Network.Funky.Types.Base
import           Network.Funky.Types.Channel
import           Network.Funky.Types.Message
import           Network.Funky.Types.User


data DiscordST = DiscordST
    { dsToken :: !Text
    , dsRates :: TVar (HashMap ByteString Int)
    }

data DiscordError
    = APIError Int Text
    | HTTPError HttpException
    | ParseError Text
    deriving (Show)

type DiscordM = ExceptT DiscordError (ReaderT DiscordST IO)

instance MonadHttp DiscordM where
    handleHttpException = throwError . HTTPError

-- We could use unsafePerformIO here, but since this is only the internal
-- representation, we only ever need to create this when we're running a bot,
-- which means it's not a pain to have this in IO
mkDiscordST :: Text -> IO DiscordST
mkDiscordST token = do
    tvar <- atomically $ newTVar empty
    return $ DiscordST token tvar


runDiscordM :: DiscordM a -> DiscordST -> IO (Either DiscordError a)
runDiscordM = runReaderT . runExceptT

testDiscordM :: Text -> DiscordM a -> IO (Either DiscordError a)
testDiscordM b dm = do
    client <- mkDiscordST b
    runDiscordM dm client

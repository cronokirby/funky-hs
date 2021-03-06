module Network.Funky
    ( module Network.Funky.API
    , module Network.Funky.Events
    , module Network.Funky.Gateway
    , module Network.Funky.Types
    , module Control.Funky.Commands
    , Client(..)
    , DiscordCommand
    , runClient
    , say
    , say_
    )
where

import           Control.Concurrent     (forkIO)
import           Control.Monad.Reader
import           Data.HashMap.Strict    hiding (map)
import           Data.Text              (Text)
import           Network.Socket         (withSocketsDo)
import           Pipes
import           Pipes.Concurrent

import           Control.Funky.Commands
import           Network.Funky.API
import           Network.Funky.Events
import           Network.Funky.Gateway
import           Network.Funky.Types


type DiscordCommand = Command (CommandM ())

data Client = Client
    { token    :: Text
    , prefix   :: Text
    , commands :: [DiscordCommand]
    , handlers :: [Handler ()]
    }

mkCMDMap :: [Command a] -> HashMap Text (Command a)
mkCMDMap = fromList . map (\x -> (commName x, x))

runClient :: Client -> IO ()
runClient (Client token prefix commands handlers) = withSocketsDo $ do
    st <- mkDiscordST token
    runGateway $ \conn -> do
        (output, input) <- spawn unbounded
        forkIO $ do
            runEffect $ gateway token conn >-> toOutput output
            performGC
        runEffect $ fromInput input
            >-> eventConsumer st handlers prefix (mkCMDMap commands)

say :: Text -> CommandM Message
say t = asks msgChannel >>= liftDM . sendMessage t

say_ :: Text -> CommandM ()
say_ = (pure () <*) . say

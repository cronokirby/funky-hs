module Network.Funky
    ( module Network.Funky.Events
    , module Network.Funky.Gateway
    , module Network.Funky.Types
    , Client(..)
    , runClient
    )
where

import           Control.Concurrent (forkIO)
import           Pipes
import           Pipes.Concurrent
import           Network.Socket     (withSocketsDo)
import           Wuss
import qualified Data.Text          as T

import Network.Funky.Events
import Network.Funky.Gateway
import Network.Funky.Types


data Client =
  Client
  { token :: T.Text
  , handlers :: [Handler ()]
  }

runClient :: Client -> IO ()
runClient (Client token handlers) = withSocketsDo $ do
  st <- mkDiscordST token
  runGateway $ \conn -> do
    (output, input) <- spawn unbounded
    forkIO $ do
      runEffect $ gateway token conn >-> toOutput output
      performGC
    runEffect $ fromInput input >-> eventConsumer st handlers

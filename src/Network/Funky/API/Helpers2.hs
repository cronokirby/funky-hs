{-# LANGUAGE
    DataKinds,
    TypeSynonymInstances
#-}
module Network.Funky.API.Helpers2
    (

    )
where

import Control.Monad.Except
import Data.ByteString    (append)
import Data.Monoid        ((<>))
import Data.Text          (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Req

import Network.Funky.Types


apiRoot :: Url 'Https
apiRoot = https "discord.app.com/api/v6"

authHeaders :: Text -> Option scheme
authHeaders token = mconcat . map (uncurry header) $
  [ ("Authorization", "Bot " `append` encodeUtf8 token)
  , ("User-Agent", "DiscordBot")
  ]

jsonHeaders :: Text -> Option scheme
jsonHeaders = (<> header "Content-Type" "application/json") . authHeaders

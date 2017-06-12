{-# LANGUAGE OverloadedStrings #-}
module Network.Funky.Types.Channel
    ( DMChannel(..)

    )
where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Function   (on)

import Network.Funky.Types.User (User)
import Network.Funky.Types.Base (Snowflake)

data DMChannel =
  DMChannel
  { dmcID :: Snowflake
  , recipient :: User
  , lastMessageId :: Snowflake
  }

instance FromJSON DMChannel where
  parseJSON = withObject "DMChannel" $ \o -> DMChannel
    <$> (read <$> o .: "id")
    <*> o .: "recipient"
    <*> o .: "last_message_id"

instance Eq DMChannel where
  (==) = (==) `on` dmcID

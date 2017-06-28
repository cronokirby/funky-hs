{-# LANGUAGE OverloadedStrings #-}
module Network.Funky.Types.Channel
    ( DMChannel(..)
    , Channel(..)
    , OverWrite(..)
    , IsChannel
    )
where

import           Data.Aeson
import           Data.Function            (on)
import           Data.Text                (Text)

import           Network.Funky.Types.Base (Snowflake)
import           Network.Funky.Types.User (User)

data DMChannel =
    DMChannel
    { dmcID            :: !Snowflake
    , dmcRecipient     :: User
    , dmcLastMessageId :: Snowflake
    }

instance FromJSON DMChannel where
    parseJSON = withObject "DMChannel" $ \o -> DMChannel
        <$> (read <$> o .: "id")
        <*> o .: "recipient"
        <*> o .: "last_message_id"

instance Eq DMChannel where
    (==) = (==) `on` dmcID


data ChannelType
    = TextChannel
    | VoiceChannel
    | PrivateChannel
    | GroupChannel

data Channel = Channel
    { chID          :: !Snowflake
    , chGuildID     :: !Snowflake
    , chName        :: Text
    , chType        :: ChannelType
    , chPosition    :: Int
    , chIsPrivate   :: Bool
    , chOverWrites  :: [OverWrite]
    , chTopic       :: Text
    , chLastMessage :: Snowflake
    , chBitRate     :: Maybe Int
    , chUserLimit   :: Maybe Int
    }

instance FromJSON Channel where
    parseJSON = withObject "Channel" $ \o -> Channel
        <$> (read <$> o .: "id")
        <*> o .: "guild_id"
        <*> o .: "name"
        <*> (parseChannelType <$> o .: "type")
        <*> o .: "position"
        <*> o .: "is_private"
        <*> o .: "permission_overwrites"
        <*> o .: "topic"
        <*> o .: "last_message_id"
        <*> o .: "bitrate"
        <*> o .: "user_limit"
      where
        parseChannelType :: Int -> ChannelType
        parseChannelType c = case c of
            0 -> TextChannel
            1 -> PrivateChannel
            2 -> VoiceChannel
            3 -> GroupChannel

instance Eq Channel where
    (==) = (==) `on` chID


data OverWrite =
    OverWrite
    { ovID    :: !Snowflake
    , ovType  :: Text
    , ovAllow :: Int
    , ovDeny  :: Int
    }

instance FromJSON OverWrite where
    parseJSON = withObject "OverWrite" $ \o -> OverWrite
        <$> (read <$> o .: "id")
        <*> o .: "type"
        <*> o .: "allow"
        <*> o .: "deny"

instance Eq OverWrite where
    (==) = (==) `on` ovID

class FromJSON a => IsChannel a

instance IsChannel Channel
instance IsChannel DMChannel

{-# LANGUAGE OverloadedStrings #-}
module Network.Funky.Types.Message
    ( Message(..)

    )
where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Function   (on)
import Data.Text       (Text)

import Network.Funky.Types.User (User)
import Network.Funky.Types.Base (Snowflake)


data Message =
  Message
  { msgID :: !Snowflake
  , msgChannel :: !Snowflake
  , msgAuthor :: User
  , msgContent :: !Text
  , msgTimestamp :: Text
  , msgEditTimestamp :: Maybe Text
  , msgTTS :: Bool
  , msgMentionEveryone :: Bool
  , msgMentions :: [User]
  , msgRoleMentions :: [Snowflake]
  , msgPinned :: Bool
  , msgWebhookID :: Maybe Text
  }
  deriving (Show)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o -> Message
    <$> (read <$> o .:  "id")
    <*> (read <$> o .:  "channel_id")
    <*> o .:  "author"
    <*> o .:  "content"
    <*> o .:  "timestamp"
    <*> o .:? "edited_timestamp"
    <*> o .:  "tts"
    <*> o .:  "mention_everyone"
    <*> o .:  "mentions"
    <*> (map read <$> o .:  "mention_roles")
    <*> o .:  "pinned"
    <*> o .:?  "webhook_id"

instance Eq Message where
  (==) = (==) `on` msgID

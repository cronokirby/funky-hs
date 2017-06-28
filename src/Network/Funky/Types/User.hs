{-# LANGUAGE OverloadedStrings #-}
module Network.Funky.Types.User
    ( User(..)
    , UserGuild(..)
    , Snowflake
    )
where

import           Data.Aeson
import           Data.Function            (on)
import           Data.Text                (Text)

import           Network.Funky.Types.Base (Snowflake)


data User = User
    { userID        :: !Snowflake
    , userName      :: Text
    , discriminator :: Text
    , userAvatar    :: Text
    , isBot         :: Bool
    , verified      :: Maybe Bool
    , userEmail     :: Maybe Text
    }
    deriving (Show)

instance FromJSON User where
    parseJSON (Object o) = User
        <$> (read <$> o .:  "id")
        <*> o .:  "username"
        <*> o .:  "discriminator"
        <*> o .:  "avatar"
        <*> o .:? "bot" .!= False
        <*> o .:? "verified"
        <*> o .:? "email"

-- Deriving this would be terribly inefficient
instance Eq User where
    (==) = (==) `on` userID


data UserGuild = UserGuild
    { ugID          :: !Snowflake
    , ugName        :: Text
    , ugIcon        :: Text
    , ugOwner       :: Bool
    , ugPermissions :: Int
    }

instance FromJSON UserGuild where
    parseJSON (Object o) = UserGuild
        <$> (read <$> o .: "id")
        <*> o .: "name"
        <*> o .: "icon"
        <*> o .: "owner"
        <*> o .: "permissions"

instance Eq UserGuild where
    (==) = (==) `on` ugID

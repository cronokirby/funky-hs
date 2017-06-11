{-# LANGUAGE OverloadedStrings #-}
module Network.Funky.Types.User
    ( User(..)
    , UserGuild(..)
    , Snowflake
    )
where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Function   (on)
import qualified Data.Text as T

import Network.Funky.Types.Base (Snowflake)


data User =
  User
  { userID :: !Snowflake
  , userName :: T.Text
  , discriminator :: T.Text
  , userAvatar :: T.Text
  , isBot :: Bool
  , verified :: Maybe Bool
  , userEmail :: Maybe T.Text
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


data UserGuild =
  UserGuild
  { ugID :: !Snowflake
  , ugName :: T.Text
  , ugIcon :: T.Text
  , ugOwner :: Bool
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

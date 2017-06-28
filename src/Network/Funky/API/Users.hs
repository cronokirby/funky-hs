{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Funky.API.Users where

import           Data.Aeson
import           Data.Text                 (Text, pack)

import           Network.Funky.API.Helpers
import           Network.Funky.API.Types
import           Network.Funky.Types


root :: Url 'Https
root = apiRoot /: "users"

getUser :: Snowflake -> DiscordM User
getUser s = api_ GET (root /~ s) mempty

getMe :: DiscordM User
getMe = api_ GET (root /: "@me") mempty

editUser :: Text -> DiscordM User
editUser name =
  api PATCH (root /: "@me") mempty $
  object ["username" .= String name]

getUserGuilds :: Maybe Period -> Int -> DiscordM [UserGuild]
getUserGuilds period limit =
    api_ GET (root /: "@me" /: "guilds") $
    "limit" =: adjust limit <> maybe mempty query period
  where
    adjust n
      | n < 0     = 0
      | n > 100   = 100
      | otherwise = n
    query (After n)  = "after" =: n
    query (Before n) = "before" =: n

leaveGuild :: Snowflake -> DiscordM ()
leaveGuild s = api_ DELETE (root /: "@me" /: "guilds" /~ s) mempty

getUserDMs :: DiscordM [DMChannel]
getUserDMs = api_ GET (root /: "@me" /: "channels") mempty

createDM :: Snowflake -> DiscordM DMChannel
createDM recipient =
  api POST (root /: "@me" /: "channels") mempty $
  object ["recipient_id" .= (String . pack $ show recipient)]

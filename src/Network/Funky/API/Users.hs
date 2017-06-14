{-# LANGUAGE OverloadedStrings #-}
module Network.Funky.API.Users where

import Data.Aeson
import Data.Text (Text, pack)
import Network.Wreq

import Network.Funky.Types
import Network.Funky.API.Helpers
import Network.Funky.API.Types


getUser :: Snowflake -> DiscordM User
getUser s = getAPI $ "users/" ++ show s

getMe :: DiscordM User
getMe = getAPI "users/@me"

editUser :: Text -> DiscordM User
editUser name =
  patchAPI "users/@me" $
  object ["username" .= String name]

getUserGuilds :: Maybe Period -> Int -> DiscordM [UserGuild]
getUserGuilds period limit = getAPI $
  "users/@me/guilds?limit="
  ++ show (adjust limit)
  ++ maybe "" query period
  where
    adjust n
      | n < 0     = 0
      | n > 100   = 100
      | otherwise = n
    query (After n)  = "&after=" ++ show n
    query (Before n) = "&before=" ++ show n

leaveGuild :: Snowflake -> DiscordM ()
leaveGuild s = deleteAPI $ "users/@me/guilds" ++ show s

getUserDMs :: DiscordM [DMChannel]
getUserDMs = getAPI "users/@me/channels"

createDM :: Snowflake -> DiscordM DMChannel
createDM recipient =
  postAPI "users/@me/channels" $
  object ["recipient_id" .= (String . pack $ show recipient)]

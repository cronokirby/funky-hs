{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Funky.API.Channels where

import           Data.Aeson
import           Data.Text                 (Text, unpack)

import           Network.Funky.API.Helpers
import           Network.Funky.Types


channelRoot :: Url 'Https
channelRoot = apiRoot /: "channels"

getChannel :: IsChannel c => Snowflake -> DiscordM c
getChannel channel = api_ GET (channelRoot /~ channel) mempty


data EditChannelArgs
    = ECName Text
    | ECPosition Int
    | ECTopic Text
    | ECBitrate Int
    | ECUserLimit Int

mkECArgs :: [EditChannelArgs] -> Value
mkECArgs = object . map (\case
    ECName t      -> "name" .= t
    ECPosition i  -> "position" .= i
    ECTopic t     -> "topic" .= t
    ECBitrate i   -> "bitrate" .= i
    ECUserLimit i -> "user_limit" .= i)

editChannel :: [EditChannelArgs] -> Snowflake -> DiscordM Channel
editChannel args channel =
    api PATCH (channelRoot /~ channel) mempty (mkECArgs args)

deleteChannel :: IsChannel c => Snowflake -> DiscordM c
deleteChannel channel =
    api_ DELETE (channelRoot /~ channel) mempty

getMessage :: Snowflake -> Snowflake -> DiscordM Message
getMessage channel message =
    api_ GET (channelRoot /~ channel /: "messages" /~ message) mempty

sendMessage :: Text -> Snowflake -> DiscordM Message
sendMessage content channel =
    api POST (channelRoot /~ channel /: "messages") mempty $
    object ["content" .= content]


createReaction :: Text -> Snowflake -> Snowflake -> DiscordM ()
createReaction emoji channel message =
    api_ PUT (channelRoot /~ channel /: "messages"
                          /~ message /: "reactions"
                          /~ emoji   /: "@me") mempty

deleteOwnReaction :: Text -> Snowflake -> Snowflake -> DiscordM ()
deleteOwnReaction emoji channel message =
    api_ DELETE (channelRoot /~ channel /: "messages"
                             /~ message /: "reactions"
                             /~ emoji   /: "@me") mempty

deleteReaction :: Text -> Snowflake -> Snowflake
               -> Snowflake -> DiscordM ()
deleteReaction emoji channel message user =
    api_ DELETE (channelRoot /~ channel /: "messages"
                             /~ message /: "reactions"
                             /~ emoji   /~ user) mempty

getReactions :: Text -> Snowflake -> Snowflake -> DiscordM [User]
getReactions emoji channel message =
    api_ GET (channelRoot /~ channel /: "messages"
                          /~ message /: "reactions"
                          /~ emoji) mempty

deleteReactions :: Snowflake -> Snowflake -> DiscordM ()
deleteReactions channel message =
    api_ DELETE
    (channelRoot /~ channel /: "messages" /~ message /: "reactions")
    mempty

editMessage :: Text -> Snowflake -> Snowflake -> DiscordM Message
editMessage content channel message =
    api PATCH (channelRoot /~ channel /: "messages" /~ message) mempty $
    object ["content" .= content]

deleteMessage :: Snowflake -> Snowflake -> DiscordM ()
deleteMessage channel message =
    api_ DELETE (channelRoot /~ channel /: "messages" /~ message) mempty

bulkDeleteMessages :: Snowflake -> [Snowflake] -> DiscordM ()
bulkDeleteMessages channel messages =
    api POST (channelRoot /~ channel /: "messages" /: "bulk-delete") mempty $
    object ["messages" .= messages]

editChannelPerms :: Text -> Int -> Int
                 -> Snowflake -> Snowflake ->DiscordM ()
editChannelPerms typ allow deny channel overwrite =
    api PUT (channelRoot /~ channel /: "permissions" /~ overwrite) mempty $
    object [ "allow" .= allow
           , "deny"  .= deny
           , "type"  .= typ
           ]

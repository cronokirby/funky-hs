{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Network.Funky.API.Channels where

import Data.Aeson
import Data.Text    (Text, unpack)
import Network.Wreq

import Network.Funky.Types
import Network.Funky.API.Helpers
import Network.Funky.API.Types


getChannel :: IsChannel c => Snowflake -> DiscordM c
getChannel channel =
  getAPI ("channels/" ++ show channel)


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
  patchAPI ("channels/" ++ show channel) $ mkECArgs args

deleteChannel :: IsChannel c => Snowflake -> DiscordM c
deleteChannel channel =
  deleteAPI ("channels/" ++ show channel)

getMessage :: Snowflake -> Snowflake -> DiscordM Message
getMessage channel message =
  getAPI ("channels/"  ++ show channel ++
          "/messages/" ++ show message)

sendMessage :: Text -> Snowflake -> DiscordM Message
sendMessage content channel =
  postAPI ("channels/" ++ show channel ++ "/messages") $
  object ["content" .= content]

createReaction :: Text -> Snowflake -> Snowflake -> DiscordM ()
createReaction emoji channel message =
  putNAPI ("channels/"   ++ show channel ++
           "/messages/"  ++ show message ++
           "/reactions/" ++ unpack emoji ++ "/@me")

deleteOwnReaction :: Text -> Snowflake -> Snowflake -> DiscordM ()
deleteOwnReaction emoji channel message =
  deleteAPI ("channels/"   ++ show channel ++
             "/messages/  "++ show message ++
             "/reactions/" ++ unpack emoji ++ "/@me")

deleteReaction :: Text -> Snowflake -> Snowflake
               -> Snowflake -> DiscordM ()
deleteReaction emoji channel message user =
  deleteAPI ("channels/"   ++ show channel ++
             "/messages/"  ++ show message ++
             "/reactions/" ++ unpack emoji ++
             "/"           ++ show user)

getReactions :: Text -> Snowflake -> Snowflake -> DiscordM [User]
getReactions emoji channel message =
  getAPI ("channels/"   ++ show channel ++
          "/messages/"  ++ show message ++
          "/reactions/" ++ unpack emoji)

deleteReactions :: Snowflake -> Snowflake -> DiscordM ()
deleteReactions channel message =
  deleteAPI ("channels/"  ++ show channel ++
             "/messages/" ++ show message ++ "/reactions")

editMessage :: Text -> Snowflake -> Snowflake -> DiscordM Message
editMessage content channel message =
  patchAPI ("channels/" ++ show channel ++
            "/messages/" ++ show message) $
 object ["content" .= content]

deleteMessage :: Snowflake -> Snowflake -> DiscordM ()
deleteMessage channel message =
  deleteAPI ("channels/" ++ show channel
             ++ "/messages/" ++ show message)

bulkDeleteMessages :: Snowflake -> [Snowflake] -> DiscordM ()
bulkDeleteMessages channel messages =
  postAPI ("channels/" ++ show channel ++ "/messages/bulk-delete") $
  object ["messages" .= messages]

editChannelPerms :: Text -> Int -> Int
                 -> Snowflake -> Snowflake ->DiscordM ()
editChannelPerms typ allow deny channel overwrite =
  putAPI ("channels/"     ++ show channel ++
          "/permissions/" ++ show overwrite) $
  object [ "allow" .= allow
         , "deny"  .= deny
         , "type"  .= typ
         ]

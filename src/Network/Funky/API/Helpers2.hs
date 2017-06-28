{-# LANGUAGE OverloadedStrings #-}
module Network.Funky.API.Helpers2
    ( getAPI
    , deleteAPI
    , postAPI
    , putAPI
    , patchAPI
    , putNAPI
    )
where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson           as A
import           Data.Aeson.Lens      (key, nth)
import           Data.ByteString      (ByteString, append)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import           Network.Wreq
import           Network.Wreq.Types   (Postable, Putable)

import           Network.Funky.Types


eitherDecode :: A.FromJSON a => LB.ByteString -> DiscordM a
eitherDecode =
  either (throwError . ParseError . T.pack) return
  . A.eitherDecode


authHeaders :: T.Text -> Options
authHeaders token =
  -- This means we should manually check status codes and provide errors
  -- inside of DiscordM
  defaults
  & checkResponse .~ (Just $ \_ _ -> return ())
  & header "Authorization" .~ [encodeUtf8 $ T.append "Bot " token]
  & header "User-Agent" .~ ["DiscordBot"]

jsonHeaders :: T.Text -> Options
jsonHeaders token =
  authHeaders token
  & header "Content-Type" .~ ["application/json"]

apiRoot :: String
apiRoot = "https://discordapp.com/api/v6/"

wrapReq :: A.FromJSON a => (T.Text -> IO (Response LB.ByteString))
        -> DiscordM a
wrapReq req = do
  token <- asks dsToken
  r <- liftIO $ req token
  handleErrors (r ^. responseStatus . statusCode) (r ^. responseBody)
  eitherDecode $ r ^. responseBody

-- TODO provide better errors here
handleErrors :: Int -> LB.ByteString -> DiscordM ()
handleErrors code body
  | code `elem` [200..299] =
    return ()
  | otherwise              =
    throwError $ APIError code (decodeUtf8 . LB.toStrict $ body)

customAPI :: A.FromJSON a => String -> String -> DiscordM a
customAPI reqType end = wrapReq $ \t ->
  customMethodWith reqType (authHeaders t) (apiRoot ++ end)

customDataAPI :: (Postable p, A.FromJSON a) =>
                 String -> String -> p -> DiscordM a
customDataAPI reqType end p = wrapReq $ \t ->
  customPayloadMethodWith reqType (jsonHeaders t) (apiRoot ++ end) p

-- Public API

putAPI :: (Putable p, A.FromJSON a) => String -> p -> DiscordM a
putAPI end p = wrapReq $ \t ->
  putWith (jsonHeaders t) (apiRoot ++ end) p

getAPI :: A.FromJSON a => String -> DiscordM a
getAPI = customAPI "GET"

deleteAPI :: A.FromJSON a => String -> DiscordM a
deleteAPI = customAPI "DELETE"

putNAPI :: A.FromJSON a => String -> DiscordM a
putNAPI = customAPI "PUT"

postAPI :: (Postable p, A.FromJSON a) => String -> p -> DiscordM a
postAPI = customDataAPI "POST"

patchAPI :: (Postable p, A.FromJSON a) => String -> p -> DiscordM a
patchAPI = customDataAPI "PATCH"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Network.Funky.API.Helpers
    ( module Network.HTTP.Req
    , module Data.Monoid
    , apiRoot
    , api
    , api_
    )
where

import           Control.Monad.Reader
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.ByteString      (ByteString, append)
import           Data.Monoid          (mconcat, mempty, (<>))
import           Data.Text            (Text)
import           Data.Text.Encoding   (encodeUtf8)
import           Network.HTTP.Req

import           Network.Funky.Types


apiRoot :: Url 'Https
apiRoot = https "discordapp.com" /: "api" /: "v6"

authHeaders :: Text -> Option scheme
authHeaders token = mconcat . map (uncurry header) $
    [ ("Authorization", "Bot " `append` encodeUtf8 token)
    , ("User-Agent", "DiscordBot")
    ]

jsonHeaders :: Option scheme
jsonHeaders = header "Content-Type" "application/json"


baseAPI :: (FromJSON r, HttpBody b, HttpMethod h,
            HttpBodyAllowed (AllowsBody h) (ProvidesBody b)) =>
           b -> h -> Url s -> Option s -> DiscordM r
baseAPI body method url options = responseBody <$> do
    token <- asks dsToken
    req method url body jsonResponse (authHeaders token <> options)

api_ :: (FromJSON r, HttpMethod h,
         HttpBodyAllowed (AllowsBody h) 'NoBody) =>
        h -> Url s -> Option s -> DiscordM r
api_ = baseAPI NoReqBody

api :: (ToJSON p, FromJSON r, HttpMethod h,
        HttpBodyAllowed (AllowsBody h) 'CanHaveBody) =>
        h -> Url s -> Option s -> p -> DiscordM r
api method url options payload =
    baseAPI (ReqBodyJson payload) method url (jsonHeaders <> options)


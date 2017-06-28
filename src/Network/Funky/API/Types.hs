module Network.Funky.API.Types
    ( Period(..)

    )
where

import           Network.Funky.Types.User (Snowflake)

data Period
     = Before Snowflake
     | After Snowflake

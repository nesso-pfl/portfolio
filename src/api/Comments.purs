module API.Comments where

import Prelude

type Comment =
    { id :: String
    , blogId :: String
    , name :: String
    , text :: String
    , date :: String
    }

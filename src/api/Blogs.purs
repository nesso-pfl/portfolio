module API.Blogs where

import Plugin.Firebase as F
import API.Comments as C

import Prelude

import Data.Maybe (Maybe(..))
import Data.Options as O
import Effect (Effect)

type Blog =
    { title :: String
    , text :: String
    , tags :: Array String
    , comments :: Array C.Comment
    , read :: Int
    , date :: String
    , public :: Boolean
    }

createBlog :: Blog -> Effect F.DocumentRef
createBlog blogOpt = do
    colRef <- F.initializeApp F.firebaseConfig Nothing >>= F.firestore >>= F.collection "blogs"
    F.add blogOpt colRef

module API.Blogs where

import Prelude
import API.Comments as C
import Plugin.Firebase as F

import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)


type Blog =
    { title :: String
    , text :: String
    , tags :: Array String
    , comments :: Array C.Comment
    , read :: Int
    , date :: String
    , public :: Boolean
    }

type Blogs = Array Blog


createBlog :: Blog -> Effect F.DocumentRef
createBlog blog = do
    colRef <- F.initializeApp F.firebaseConfig Nothing >>= F.firestore >>= F.collection "blogs"
    F.add blog colRef

getBlog :: Int -> Aff Blogs
getBlog n = do
    colRef <- liftEffect $
        F.initializeApp F.firebaseConfig Nothing >>= F.firestore >>= F.collection "blogs" >>= F.limit n
    ss <- liftAff $ F.get Nothing colRef
    liftEffect $ F.docs ss >>= traverse F.data'

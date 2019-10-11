module API.Blogs where

import Plugin.Firebase as F
import API.Comments as C

import Prelude

import Data.Maybe (Maybe(..))
import Data.Options as O
import Effect.Class.Console (log)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Query.EventSource as ES

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
createBlog blog = do
    colRef <- F.initializeApp F.firebaseConfig Nothing >>= F.firestore >>= F.collection "blogs"
    F.add blog colRef

{-
getBlog :: Int -> Aff Unit
getBlog n = do
    colRef <- liftEffect $ F.initializeApp F.firebaseConfig Nothing >>= F.firestore >>= F.collection "blogs"
    F.get Nothing colRef \ss -> do
        d <- liftEffect $ F.docs ss >>= F.data'
        H.put d
-}

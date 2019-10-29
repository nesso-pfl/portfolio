module API.Blogs where

import Prelude
import API.Comments as C
import Plugin.Firebase as F

import Data.Array (concatMap, filter, head, length)
import Data.DateTime as DT
import Data.Formatter.DateTime as FD
import Data.Maybe (Maybe(..))
import Data.JSDate as D
import Data.List (fromFoldable)
import Data.Traversable (traverse)
import Data.Tuple as T
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)


type Blog =
    { id :: String
    , title :: String
    , text :: String
    , tags :: Tags
    , comments :: Array C.Comment
    , read :: Int
    , date :: F.Timestamp
    , public :: Boolean
    }

type Blogs = Array Blog
type Tag = String
type Tags = Array String
type TagInfo = Array (T.Tuple String Int)

initBlog :: Blog
initBlog =
    { id: ""
    , title: ""
    , text: ""
    , tags: []
    , comments: []
    , read: 0
    , date: F.now
    , public: true
    }

createBlog :: Blog -> Effect F.DocumentRef
createBlog blog = do
    colRef <- F.initializeApp F.firebaseConfig Nothing >>= F.firestore >>= F.collection "blogs"
    F.add blog colRef

getBlog :: Int -> Aff Blogs
getBlog n = do
    colRef <- liftEffect $ F.initializeApp F.firebaseConfig Nothing >>= F.firestore >>= F.collection "blogs"
    ss <- F.get Nothing colRef
    liftEffect $ F.docs ss >>= traverse \d -> pure $ (F.data' d) { id = F.id d }

tallyTags :: Blogs -> TagInfo
tallyTags s = hoho $ concatMap _.tags s

hoho :: Tags -> TagInfo
hoho tags = case head tags of
     Just t -> [T.Tuple t (length $ filter ((==) t) tags) ] <> hoho (filter ((/=) t) tags)
     Nothing -> []

showDate :: F.Timestamp -> String
showDate t = case D.toDateTime $ F.toDate t of
    Just dt -> FD.format formatter dt
    Nothing -> ""
    where formatter :: FD.Formatter
          formatter = fromFoldable
              [ FD.YearFull
              , FD.Placeholder "-"
              , FD.MonthTwoDigits
              , FD.Placeholder "-"
              , FD.DayOfMonthTwoDigits
              ]

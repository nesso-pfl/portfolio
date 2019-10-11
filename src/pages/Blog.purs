module Page.Blog where

import Plugin.HalogenR
import API.Blogs as B

import Plugin.Firebase as F
import Plugin.MarkdownIt
import Data.Maybe (Maybe(..))
import Data.Array (length)
import Data.Const (Const)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML (ClassName(..), HTML, div, h1_, text)
import Halogen.HTML.Properties as HP
import Prelude


data Action
    = GetBlogs
    | Fa

ui :: H.Component HTML (Const Unit) Unit Void Aff
ui = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just GetBlogs
        }
    }

type State = Array B.Blog

type Slot = H.Slot (Const Unit) Void

initialState :: Unit -> Array B.Blog
initialState _ = []

render :: State -> H.ComponentHTML Action () Aff
render st =
    divC "page blog"
        [ divC1 "left-area" $ text "Recent Act and Search Form"
        , divC "central-area" $ blogMain st
        , divC1 "right-area" $ text "Adds"
        ]

    where blogMain :: forall p. State -> Array (HTML p Action)
          blogMain = map \b -> divC "blog"
              [ divC1 "header" ( divC1 "title" $ text b.title )
              , divC1 "main" ( divC1 "text" $ text b.text )
              , divC "footer"
                  [ divC "tags" $ map text b.tags
                  , divC1 "date" $ text b.date
                  -- , divC1 "comments" $ text <<< show <<< length $ b.comments
                  ]
              ]

handleAction :: Action -> H.HalogenM State Action () Void Aff Unit
handleAction = case _ of
    GetBlogs -> do
       colRef <- H.liftEffect $ F.initializeApp F.firebaseConfig Nothing >>= F.firestore >>= F.collection "blogs"
       ss <- liftAff $ F.get Nothing colRef
       H.liftEffect $ log "hoho"
       d <- H.liftEffect $ F.docs ss >>= traverse F.data'
       H.liftEffect $ log $ show d
       H.put d

    Fa -> do
       H.liftEffect $ log "hoho"
       pure unit

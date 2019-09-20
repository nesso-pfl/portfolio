module Page.Blog where

import Plugin.HalogenR

import Plugin.MarkdownIt
import Data.Array (length)
import Data.Const (Const)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML (ClassName(..), HTML, div, h1_, text)
import Halogen.HTML.Properties as HP
import Prelude


ui :: H.Component HTML (Const Unit) Unit Void Aff
ui = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
    }

type Comment =
    { id :: String
    , blogId :: String
    , name :: String
    , text :: String
    , date :: String
    }

type Blog =
    { id :: String
    , title :: String
    , text :: String
    , tags :: Array String
    , comments :: Array Comment
    , read :: Int
    , date :: String
    }

type State = Array Blog

type Slot = H.Slot (Const Unit) Void

initialState :: Unit -> Array Blog
initialState _ = []

render :: State -> H.ComponentHTML Unit () Aff
render st =
    divC "page blog"
        [ divC1 "left-area" $ text "Recent Act and Search Form"
        , divC "central-area" $ blogMain st
        , divC1 "right-area" $ text "Adds"
        ]

    where blogMain :: forall p a. Array Blog -> Array (HTML p a)
          blogMain = map \b -> divC "blog"
              [ divC1 "header" ( divC1 "title" $ text b.title )
              , divC1 "main" ( divC1 "text" $ text b.text )
              , divC "footer"
                  [ divC "tags" $ map text b.tags
                  , divC1 "date" $ text b.date
                  , divC1 "comments" $ text <<< show <<< length $ b.comments
                  ]
              ]

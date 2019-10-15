module Page.Blog where

import Prelude
import API.Blogs as B
import Plugin.HalogenR (divC, divC1)
import Plugin.Firebase as F
import Plugin.MarkdownIt as MD

import Data.Maybe (Maybe(..))
import Data.Const (Const)
import Data.Options ((:=))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect (Effect)

import Halogen as H
import Halogen.HTML (HTML, text)
import Html.Renderer.Halogen as RH


data Action
    = GetBlogs

ui :: H.Component HTML (Const Unit) Unit Void Aff
ui = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just GetBlogs
        }
    }

type State = B.Blogs

type Slot = H.Slot (Const Unit) Void

initialState :: Unit -> B.Blogs
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
              , divC1 "main" ( divC1 "text" $ RH.render_ b.text )
              , divC "footer"
                  [ divC "tags" $ map text b.tags
                  , divC1 "date" $ text b.date
                  -- , divC1 "comments" $ text <<< show <<< length $ b.comments
                  ]
              ]

handleAction :: Action -> H.HalogenM State Action () Void Aff Unit
handleAction = case _ of
    GetBlogs -> do
       blogs <- liftAff $ B.getBlog 5
       renderedBlogs <- H.liftEffect $ renderToHtml blogs
       H.put renderedBlogs

renderToHtml :: B.Blogs -> Effect B.Blogs
renderToHtml blogs = do
    let opt = MD.html    := true
           <> MD.linkify := true
    md <- MD.newMarkdownIt MD.CommonMark opt
    blogs # traverse \b -> do
        renderedText <- MD.render md b.text
        pure $ b { text = renderedText }

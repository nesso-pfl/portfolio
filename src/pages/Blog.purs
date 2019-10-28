module Page.Blog where

import Prelude
import API.Blogs as B
import Plugin.HalogenR (divC, divC1)
import Plugin.MarkdownIt as MD

import Data.Maybe (Maybe(..))
import Data.Const (Const)
import Data.Options ((:=))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
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
        [ divC "left-area"
            [ divC "recent-posts"
                [ divC1 "title" $ text "Recent Posts"
                , divC "contents" $ recentPosts st
                ]
            , divC "recent-comments"
                [ divC1 "title" $ text "Recent Comments"
                , divC "contents" $ recentComments st
                ]
            ]
        , divC "central-area" $ blogMain st
        , divC "right-area"
            [ divC "tags"
                [ divC1 "title" $ text "Tags"
                , divC "contents" $ tags st
                ]
            , divC "search"
                []
            ]
        ]

    where blogMain :: ∀ p. State -> Array (HTML p Action)
          blogMain = map \b -> divC "blog"
              [ divC1 "header" ( divC1 "title" $ text b.title )
              , divC1 "main" ( divC1 "text" $ RH.render_ b.text )
              , divC "footer"
                  [ divC "tags" $ map text b.tags
                  , divC1 "date" $ text $ B.showDate b.date
                  -- , divC1 "comments" $ text <<< show <<< length $ b.comments
                  ]
              ]
          recentPosts :: ∀ p. State -> Array (HTML p Action)
          recentPosts = map \b -> divC "content" []
          recentComments :: ∀ p. State -> Array (HTML p Action)
          recentComments = map \b -> divC "content" []
          tags :: ∀ p. State -> Array (HTML p Action)
          tags = map \b -> divC "content" []

handleAction :: Action -> H.HalogenM State Action () Void Aff Unit
handleAction = case _ of
    GetBlogs -> do
       blogs <- H.liftAff $ B.getBlog 5
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

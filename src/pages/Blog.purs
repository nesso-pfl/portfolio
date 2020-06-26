module Page.Blog where

import Prelude
import API.Blogs as B
import Plugin.HalogenR (setTitle, divC, divCE, divC1, h1C1, h2C1, spanC1, buttonCE1)
import Plugin.MarkdownIt as MD

import Data.Maybe (Maybe(..))
import Data.Const (Const)
import Data.Options ((:=))
import Data.String.Common (joinWith)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect (Effect)
import Effect.Class.Console (log)

import Halogen as H
import Halogen.HTML (HTML, text)
import Html.Renderer.Halogen as RH


data Action
    = GetBlogs
    | AddRead String
    | GoArticle String

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
                [ h2C1 "title" $ text "Recent Posts"
                , divC "contents" $ recentPosts st
                ]
            , divC "recent-comments"
                [ h2C1 "title" $ text "Recent Comments"
                , divC "contents" $ recentComments st
                ]
            ]
        , divC "central-area" $ blogMain st
        , divC "right-area"
            [ divC "tags"
                [ h2C1 "title" $ text "Tags"
                , divC "contents" $ tags st
                ]
            , divC "search"
                []
            ]
        ]

    where blogMain :: ∀ p. State -> Array (HTML p Action)
          blogMain = map \b -> divC "blog md"
              [ divC "title"
                  [ h1C1 "content" $ text b.title
                  , spanC1 "date" $ text $ B.showDate b.date
                  ]
              , divC1 "tags" $ text (joinWith ", " b.tags)
              , divC1 "main" ( divC1 "text" $ RH.render_ b.text )
              , buttonCE1 "btn-read" (AddRead b.id) (text "読んだよ！")
              ]
          recentPosts :: ∀ p. State -> Array (HTML p Action)
          recentPosts = map \b -> divCE "content" (GoArticle b.id)
              [ spanC1 "title" $ text b.title
              , divC1 "tags" $ text (joinWith "," b.tags)
              , divC1 "date" $ text $ B.showDate b.date
              ]
          recentComments :: ∀ p. State -> Array (HTML p Action)
          recentComments = map \b -> divC "content" []
          tags :: ∀ p. State -> Array (HTML p Action)
          tags = map \b -> divC "content" []

handleAction :: Action -> H.HalogenM State Action () Void Aff Unit
handleAction = case _ of
    GetBlogs -> do
       H.liftEffect $ setTitle "ブログ - nesso-pfl"
       blogs <- H.liftAff $ B.getBlog 0
       renderedBlogs <- H.liftEffect $ renderToHtml blogs
       H.put renderedBlogs
    AddRead id -> do
       pure unit
    GoArticle id -> do
       pure unit

renderToHtml :: B.Blogs -> Effect B.Blogs
renderToHtml blogs = do
    let opt = MD.html    := true
           <> MD.linkify := true
    md <- MD.newMarkdownIt MD.CommonMark opt
    blogs # traverse \b -> do
        renderedText <- MD.render md b.text
        pure $ b { text = renderedText }

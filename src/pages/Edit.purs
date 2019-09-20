module Page.Blog.Edit where

import Ace (ace, edit)
import Ace.Document (getAllLines, onChange)
import Ace.Editor (getSession)
import Ace.EditSession (getDocument)
import API.Blogs as B
import Plugin.HalogenR
import Plugin.Vim (addKeydownEvent)

import Prelude

import Data.Options ((:=))
import Plugin.MarkdownIt as MD
import Data.Const (Const)
import Data.DateTime.Instant (unInstant, toDateTime)
import Data.List (foldl)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML (HTML, input, text, textarea)
import Halogen.HTML.Events (onValueChange, onValueInput)
import Html.Renderer.Halogen as RH


ui :: H.Component HTML (Const Unit) Unit Message Aff
ui = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just InitAce
        }
    }

type Message = String

type State = 
    { blog :: B.Blog
    , renderedText :: String
    }

type Slot = H.Slot (Const Unit) Message

data Action
    = ChangeTitle String
    | InputText String
    | SaveBlog
    | InitAce
    | AddKeyEvent

initialState :: Unit -> State
initialState _ =
    { blog: { title: ""
            , text: ""
            , tags: []
            , comments: []
            , read: 0
            , date: ""
            , public: true
            }
    , renderedText: ""
    }

render :: State -> H.ComponentHTML Action () Aff
render st =
    divC "page blog-edit"
        [ divC1 "title" $ input [ onValueChange $ Just <<< ChangeTitle ]
        , divC "edit-area"
            [ divCI1 "input" "editor-ace" $ text ""
            , divC1 "rendered" $ RH.render_ st.renderedText
            ]
        , buttonCE1 "btn-save" SaveBlog $ text "保存"
        ]

handleAction :: Action -> H.HalogenM State Action () Message Aff Unit
handleAction = case _ of
    ChangeTitle v -> do
        H.modify_ (_ { blog { title = v } })
    InputText v -> do
        let opt = MD.html    := true
               <> MD.linkify := true
        md <- liftEffect $ MD.newMarkdownIt MD.CommonMark opt
        renderedText <- liftEffect $ MD.render md v
        H.modify_ (_ { blog { text = v }, renderedText = renderedText })
    SaveBlog -> do
        now_ <- liftEffect now
        liftEffect $ log <<< show <<< toDateTime $ now_
        H.modify_ (_ { blog { date = show (unInstant now_) } })
        blog <- H.gets _.blog
        _ <- liftEffect $ B.createBlog blog
        H.raise "/blog"
    InitAce -> do
        editor <- liftEffect $ edit "editor-ace" ace
        doc <- liftEffect $ getSession editor >>= getDocument
        liftEffect $ onChange doc \_ -> do
            launchAff_ $ H.modify_ (_ { blog { text = "" }, renderedText = ""})
        pure unit
    AddKeyEvent -> do
        liftEffect addKeydownEvent

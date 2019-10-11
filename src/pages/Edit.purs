module Page.Blog.Edit where

import Ace (Editor, ace, edit)
import Ace.Config as AC
import Ace.Document (getAllLines, onChange)
import Ace.Document as AD
import Ace.Editor (getSession)
import Ace.Editor as AE
import Ace.EditSession (getDocument)
import Ace.EditSession as AS
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
import Data.String.Common (joinWith)
import Effect.Aff (Aff, launchAff_)
import Effect.Console (log)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML (HTML, input, text, textarea)
import Halogen.HTML.Events (onValueChange, onValueInput)
import Halogen.Query.EventSource as ES
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
    , editor :: Maybe Editor
    }

type Slot = H.Slot (Const Unit) Message

data Action
    = ChangeTitle String
    | InputText
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
    , editor: Nothing
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
        H.modify_ $ _ { blog { title = v } }
    InputText -> do
        e <- H.gets _.editor
        case e of
            Just e -> do
                doc <- H.liftEffect $ getSession e >>= getDocument
                t <- H.liftEffect $ AE.getValue e
                let opt = MD.html    := true
                       <> MD.linkify := true
                md <- H.liftEffect $ MD.newMarkdownIt MD.CommonMark opt
                renderedText <- H.liftEffect $ MD.render md t
                H.modify_ $ _ { blog { text = t }, renderedText = renderedText }
            Nothing -> pure unit
    SaveBlog -> do
        now_ <- H.liftEffect now
        H.liftEffect $ log <<< show <<< toDateTime $ now_
        H.modify_ (_ { blog { date = show (unInstant now_) } })
        blog <- H.gets _.blog
        _ <- H.liftEffect $ B.createBlog blog
        H.raise "/blog"
    -- If using ace editor, switch initialize to this
    InitAce -> do
        editor <- H.liftEffect $ edit "editor-ace" ace
        -- _ <- H.liftEffect $ AC.set AC.basePath "path"
        -- H.liftEffect $ AE.setKeyboardHandler "ace/keyboard/vim" editor
        H.modify_ $ _ { editor = Just editor }
        doc <- H.liftEffect $ getSession editor >>= getDocument
        void $ H.subscribe $ ES.effectEventSource \emitter -> do
            onChange doc (\_ -> ES.emit emitter $ InputText)
            pure mempty
        pure unit
    -- If using my vim plugin, switch initialize to this
    AddKeyEvent -> do
        H.liftEffect addKeydownEvent

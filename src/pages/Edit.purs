module Page.Blog.Edit where

import Prelude
import API.Blogs as B
import Plugin.HalogenR (buttonCE1, divC, divC1, divCI1, inputtextE)
import Plugin.MarkdownIt as MD
import Plugin.Vim (addKeydownEvent)

import Data.Options ((:=))
import Data.Const (Const)
import Data.DateTime.Instant (unInstant, toDateTime)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Effect.Now (now)
import Web.Event.Internal.Types (Event)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key, fromEvent)

import Ace (Editor, ace, edit)
import Ace.Document as AD
import Ace.Editor (getSession)
import Ace.Editor as AE
import Ace.EditSession as AS

import Halogen as H
import Halogen.HTML (HTML, input, text)
import Halogen.HTML.Events (onValueChange)
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
    , tags :: Array String
    , inputTag :: String
    , editor :: Maybe Editor
    }

type Slot = H.Slot (Const Unit) Message

data Action
    = ChangeTitle String
    | InputText
    | InputTag Event
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
    , tags: []
    , inputTag: ""
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
        , divC "tags" $ tags st <> [ inputtextE st.inputTag InputTag ]
        , buttonCE1 "btn-save" SaveBlog $ text "保存"
        ]
    where tags :: ∀ p i. State -> Array (HTML p i)
          tags st = st.tags # map \t -> divC1 "tag" $ text t


handleAction :: Action -> H.HalogenM State Action () Message Aff Unit
handleAction = case _ of
    ChangeTitle v -> do
        H.modify_ $ _ { blog { title = v } }
    InputText -> do
        e <- H.gets _.editor
        case e of
            Just e -> do
                doc <- H.liftEffect $ getSession e >>= AS.getDocument
                t <- H.liftEffect $ AE.getValue e
                let opt = MD.html    := true
                       <> MD.linkify := true
                md <- H.liftEffect $ MD.newMarkdownIt MD.CommonMark opt
                renderedText <- H.liftEffect $ MD.render md t
                H.modify_ $ _ { blog { text = t }, renderedText = renderedText }
            Nothing -> pure unit
    InputTag e -> do
        keyE <- fromEvent e
        case keyE of
            Just keyE' -> do
                H.liftEffect $ log $ key keyE'
                pure unit
            Nothing ->
                pure unit
    SaveBlog -> do
        now_ <- H.liftEffect now
        H.modify_ $ _ { blog { date = show (unInstant now_) } }
        blog <- H.gets _.blog
        _ <- H.liftEffect $ B.createBlog blog
        H.raise "/blog"
    -- If using ace editor, switch initialize to this
    InitAce -> do
        editor <- H.liftEffect $ edit "editor-ace" ace
        -- _ <- H.liftEffect $ AC.set AC.basePath "path"
        -- H.liftEffect $ AE.setKeyboardHandler "ace/keyboard/vim" editor
        H.modify_ $ _ { editor = Just editor }
        doc <- H.liftEffect $ getSession editor >>= AS.getDocument
        void $ H.subscribe $ ES.effectEventSource \emitter -> do
            AD.onChange doc (\_ -> ES.emit emitter $ InputText)
            pure mempty
        pure unit
    -- If using my vim plugin, switch initialize to this
    AddKeyEvent -> do
        H.liftEffect addKeydownEvent

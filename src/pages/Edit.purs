module Page.Blog.Edit where

import Prelude
import API.Blogs as B
import Plugin.Firebase as F
import Plugin.HalogenR (buttonCE1, divC, divC1, divCI1, inputtextIE, spanC1, withDomId)
import Plugin.MarkdownIt as MD
import Plugin.Vim (addKeydownEvent)

import Data.Options ((:=))
import Data.Const (Const)
import Data.Maybe (Maybe(..), isJust)
import Data.String.Common (null)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
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
import Web.HTML.HTMLElement (focus)


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
    , inputTag :: String
    , editor :: Maybe Editor
    }

type Slot = H.Slot (Const Unit) Message

data Action
    = ChangeTitle String
    | InputText
    | InputTag String
    | EnterTag KeyboardEvent
    | SaveBlog
    | InitAce
    | AddKeyEvent

initialState :: Unit -> State
initialState _ =
    { blog: B.initBlog
    , renderedText: ""
    , inputTag: ""
    , editor: Nothing
    }

render :: State -> H.ComponentHTML Action () Aff
render st =
    divC "page blog-edit"
        [ divC "left-area"
            [ divC "title"
                [ spanC1 "pretext" $ text "Title: "
                , input [ onValueChange $ Just <<< ChangeTitle ]
                ]
            , divC "edit-area" [ divCI1 "input" "editor-ace" $ text "" ]
            , divC "tags" $
                [ spanC1 "pretext" $ text "Tags: " ]
                <> tags st
                <> [ inputtextIE "tags-input" InputTag EnterTag ]
            , buttonCE1 "btn-save" SaveBlog $ text "保存"
            ]
        , divC "right-area" [ divC1 "rendered" $ RH.render_ st.renderedText ]
        ]
    where tags :: ∀ p i. State -> Array (HTML p i)
          tags st = st.blog.tags # map \t -> spanC1 "tag" $ text (t <> ", ")


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
    InputTag s -> do
        H.modify_ $ _ { inputTag = s }
    EnterTag e -> do
        case key e of
            "Enter" -> do
                inputTag <- H.gets _.inputTag
                if not $ null inputTag
                then do
                    tags <- H.gets _.blog.tags
                    H.modify_ $ _ { inputTag = "", blog { tags = tags <> [ inputTag ] } }
                    H.liftEffect $ withDomId "tags-input" focus
                else pure unit
            a -> pure unit
    SaveBlog -> do
        H.modify_ $ _ { blog { date = F.now } }
        blog <- H.gets _.blog
        _ <- H.liftEffect $ B.createBlog blog
        H.raise "/blog"
    -- If using ace editor, switch initialize to this
    InitAce -> do
        signInInfo <- H.liftEffect $ F.initializeApp F.firebaseConfig Nothing >>= F.auth >>= F.currentUser
        case signInInfo of
            Nothing -> do
                H.liftEffect $ log "oh no"
                pure unit
            Just user -> do
                -- H.liftEffect $ log user
                H.liftEffect $ log "hoho"
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

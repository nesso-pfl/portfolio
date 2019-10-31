module Page.Auth where

import Prelude
import Data.Const (Const)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Plugin.HalogenR (divC, buttonCE1, textForm)

import Halogen as H
import Halogen.HTML (HTML, text)

data Action
    = IdInput String
    | PasswordInput String
    | Login

type State = 
    { id :: String
    , password :: String
    }

type Slot = H.Slot (Const Unit) Void

ui :: H.Component HTML (Const Unit) Unit Void Aff
ui = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
    }

initialState :: Unit -> State
initialState _ =
    { id: ""
    , password: ""
    }

render :: State -> H.ComponentHTML Action () Aff
render _ = divC "page auth"
    [ textForm "id" "ID" "8uR4yhfuIjq3r9UIbfiUh3" IdInput
    , textForm "password" "Password" "8uR4yhfuIjq3r9UIbfiUh3" PasswordInput
    , buttonCE1 "btn-login" Login $ text "ログイン"
    ]

handleAction :: Action -> H.HalogenM State Action () Void Aff Unit
handleAction = case _ of
    IdInput s -> do
        H.modify_ $ _ { id = s }
    PasswordInput s -> do
        H.modify_ $ _ { password = s }
    Login -> do
        log "hoho"
        pure unit

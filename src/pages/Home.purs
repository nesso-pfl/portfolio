module Page.Home where

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Plugin.HalogenR (divC1, setTitle)
import Prelude


ui :: H.Component HH.HTML (Const Unit) Unit Void Aff
ui = H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just SetTitle
        }
    }

data Action
    = SetTitle

type Slot = H.Slot (Const Unit) Void

render :: Unit -> H.ComponentHTML Action () Aff
render st =
    HH.div_
        [ HH.h2_ [ HH.text "nesso Profile"]
        , HH.p_ [ HH.text "主にフロントエンドエンジニアを生業にしているが、一応バックエンドや AWS でのインフラ構築も見ることができる。" ]
        , HH.p_
            [ HH.text $ "静的型付き言語・関数型言語に強い関心がある。一方で動的型付き言語や、"
                     <> "関数型の流儀で記述するための機能に乏しい言語はあまり触りたがらない。"
            ]
        , HH.p_
            [ HH.text $ "最近では Rust で WebSocket サーバーを立てようとして挫折したり、webpack のソースコードを完全に理解しようとして挫折したり、"
                     <> "TaPL を完全に理解しようとして挫折している。"
            ]
        , HH.p_ [ HH.text $ "GitHub: https://github.com/nesso-pfl" ]
        ]

handleAction :: Action -> H.HalogenM Unit Action () Void Aff Unit
handleAction = case _ of
    SetTitle -> do
       H.liftEffect $ setTitle "nesso-pfl"
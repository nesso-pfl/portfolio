module Page.Home where

import Control.Monad.State as S
import Data.Const (Const)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Prelude


ui :: H.Component HH.HTML (Const Unit) Unit Void Aff
ui = H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval $ H.defaultEval
    }

data Action
    = Hoho

type Slot = H.Slot (Const Unit) Void

render :: Unit -> H.ComponentHTML Action () Aff
render st =
    HH.div_
        [ HH.h1_ [ HH.text "Home" ]
        , HH.h1_ [ HH.text $ show st]
        ]

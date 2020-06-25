module Page.Home where

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Plugin.HalogenR (setTitle)
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
        [ HH.h1_ [ HH.text "Home" ]
        , HH.h1_ [ HH.text $ show st]
        ]

handleAction :: Action -> H.HalogenM Unit Action () Void Aff Unit
handleAction = case _ of
    SetTitle -> do
       H.liftEffect $ setTitle "nesso-pfl"
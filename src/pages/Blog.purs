module Blog where

import Data.Const (Const)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Prelude


ui :: H.Component HH.HTML (Const Unit) Unit Void Aff
ui = H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval
    }

type State = Int

type Slot = H.Slot

initialState :: Int
initialState = 0

render :: Unit -> H.ComponentHTML Unit () Aff
render _ =
    HH.div_
        [ HH.h1_ [ HH.text "Blog" ] ]

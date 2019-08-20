module Home where

import Halogen as H
import Halogen.HTML as HH
import Prelude


ui :: forall i o m. H.Component HH.HTML i o m
ui = H.mkComponent
    { render
    }


render :: forall m. Unit -> H.ComponentHTML Unit Void m
render st =
    HH.div_
        [ HH.h1_ [ HH.text "Home" ] ]

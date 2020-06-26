module Component.Base where

import Prelude (($))
import Halogen.HTML (ClassName(..), HTML, div)
import Halogen.HTML.Properties as HP

base :: ∀ a i. Array (HTML a i) -> HTML a i
base p =
  div [HP.class_ $ ClassName "base-wrapper"] [
    div [HP.class_ $ ClassName "base-inner"] p
  ]

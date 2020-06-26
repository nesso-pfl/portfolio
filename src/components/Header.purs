module Component.Header where

import Plugin.HalogenR

import Prelude (Unit, Void, const, map, unit, (#), ($), (<>))
import Data.Const (Const)
import Data.String.Common (toLower)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML (ClassName(..), HTML, header, link, text)
import Halogen.HTML.Properties as HP

contents :: Array String
contents =
    [ "Home"
    , "Blog"
    ]

data Action
    = Goto String

type Slot index = H.Slot (Const Void) Message index

type Message = String

ui :: ∀ q i. H.Component HTML q i Message Aff
ui = H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        }
    }

render :: ∀ s. Unit -> H.ComponentHTML Action s Aff
render _ =
    header [ HP.class_ $ ClassName "header" ]
        (headerContents <> [ link [ HP.href "/assets/sass/header.sass" ] ])
    where headerContents :: ∀ p. Array (HTML p Action)
          headerContents = contents # map \c ->
              divCE1 ("content " <> toLower c) (Goto c) $ text c

handleAction :: ∀ s. Action -> H.HalogenM Unit Action s Message Aff Unit
handleAction = case _ of
    Goto "Home" -> do
        H.raise $ "/"
    Goto s -> do
        H.raise $ "/" <> toLower s

module Plugin.HalogenR 
    ( divC
    , divC1
    , divCE1
    , divCI1
    , inputtextE
    , textareaIE
    , buttonCE1
    )
    where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen.HTML (HTML, ClassName(..), div, button, input, textarea)
import Halogen.HTML.Properties (class_, id_, value)
import Halogen.HTML.Core (PropName(..))
import Halogen.HTML.Events (onClick, onInput, onValueInput)
import Web.Event.Internal.Types (Event)

divC :: ∀ p i. String -> Array (HTML p i) -> HTML p i
divC cls es = div [ class_ $ ClassName cls ] es

divC1 :: ∀ p i. String -> HTML p i -> HTML p i
divC1 cls e = div [ class_ $ ClassName cls ] [ e ]

divC2 :: ∀ p i. String -> HTML p i -> HTML p i -> HTML p i
divC2 cls e1 e2 = div [ class_ $ ClassName cls ] [ e1, e2 ]

divCE1 :: ∀ p i. String -> i -> HTML p i -> HTML p i
divCE1 cls action e =
    div [ class_ $ ClassName cls, onClick \_ -> Just action ] [ e ]

divCI1 :: ∀ p i. String -> String -> HTML p i -> HTML p i
divCI1 cls i e = div [ class_ $ ClassName cls, id_ i ] [ e ]

inputtextE :: ∀ p i. String -> (Event -> i) -> HTML p i
inputtextE s action = input [ value $ s, onInput $ Just <<< action ]

textareaIE :: ∀ p i. String -> (String -> i) -> HTML p i
textareaIE i action = textarea [ id_ i, onValueInput $ Just <<< action ]

buttonCE1 :: ∀ p i. String -> i -> HTML p i -> HTML p i
buttonCE1 cls action e =
    button [ class_ $ ClassName cls, onClick \_ -> Just action ] [ e ]
    

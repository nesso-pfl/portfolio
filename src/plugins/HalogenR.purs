module Plugin.HalogenR 
    ( divC
    , divC1
    , divCE1
    , divCI1
    , textareaIE
    , buttonCE1
    )
    where

import Prelude
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.String.Common (toLower)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML (HTML, ClassName(..), div, button, textarea)
import Halogen.HTML.Properties (class_, id_)
import Halogen.HTML.Events (onClick, onValueInput)

divC :: forall p i. String -> Array (HTML p i) -> HTML p i
divC cls es = div [ class_ $ ClassName cls ] es

divC1 :: forall p i. String -> HTML p i -> HTML p i
divC1 cls e = div [ class_ $ ClassName cls ] [ e ]

divC2 :: forall p i. String -> HTML p i -> HTML p i -> HTML p i
divC2 cls e1 e2 = div [ class_ $ ClassName cls ] [ e1, e2 ]

divCE1 :: forall p i. String -> i -> HTML p i -> HTML p i
divCE1 cls action e =
    div [ class_ $ ClassName cls, onClick \_ -> Just action ] [ e ]

divCI1 :: forall p i. String -> String -> HTML p i -> HTML p i
divCI1 cls i e = div [ class_ $ ClassName cls, id_ i ] [ e ]

textareaIE :: forall p i. String -> (String -> i) -> HTML p i
textareaIE i action = textarea [ id_ i, onValueInput $ Just <<< action ]

buttonCE1 :: forall p i. String -> i -> HTML p i -> HTML p i
buttonCE1 cls action e =
    button [ class_ $ ClassName cls, onClick \_ -> Just action ] [ e ]
    

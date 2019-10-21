module Plugin.HalogenR 
    ( divC
    , divC1
    , spanC1
    , divCE1
    , divCI1
    , inputtextIE
    , textareaIE
    , buttonCE1
    , withDomId
    )
    where

import Prelude
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect (Effect)
import Halogen.HTML (HTML, ClassName(..), div, button, input, span, textarea)
import Halogen.HTML.Properties (class_, id_, value)
import Halogen.HTML.Core (PropName(..))
import Halogen.HTML.Events (onClick, onKeyDown, onInput, onValueInput)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLElement (HTMLElement, blur, focus, fromElement)
import Partial.Unsafe (unsafePartial)
import Web.Event.Internal.Types (Event)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

divC :: ∀ p i. String -> Array (HTML p i) -> HTML p i
divC cls es = div [ class_ $ ClassName cls ] es

divC1 :: ∀ p i. String -> HTML p i -> HTML p i
divC1 cls e = div [ class_ $ ClassName cls ] [ e ]

spanC1 :: ∀ p i. String -> HTML p i -> HTML p i
spanC1 cls e = span [ class_ $ ClassName cls ] [ e ]

divC2 :: ∀ p i. String -> HTML p i -> HTML p i -> HTML p i
divC2 cls e1 e2 = div [ class_ $ ClassName cls ] [ e1, e2 ]

divCE1 :: ∀ p i. String -> i -> HTML p i -> HTML p i
divCE1 cls action e =
    div [ class_ $ ClassName cls, onClick \_ -> Just action ] [ e ]

divCI1 :: ∀ p i. String -> String -> HTML p i -> HTML p i
divCI1 cls i e = div [ class_ $ ClassName cls, id_ i ] [ e ]

inputtextIE :: ∀ p i. String -> (String -> i) -> (KeyboardEvent -> i) -> HTML p i
inputtextIE i action1 action2 =
    input [ id_ i, onValueInput $ Just <<< action1, onKeyDown $ Just <<< action2 ]

textareaIE :: ∀ p i. String -> (String -> i) -> HTML p i
textareaIE i action = textarea [ id_ i, onValueInput $ Just <<< action ]

buttonCE1 :: ∀ p i. String -> i -> HTML p i -> HTML p i
buttonCE1 cls action e =
    button [ class_ $ ClassName cls, onClick \_ -> Just action ] [ e ]
    
withDomId :: String -> (HTMLElement -> Effect Unit) -> Effect Unit
withDomId s f = do
    dom <- window >>= document >>= toNonElementParentNode >>> getElementById s
    let el = dom >>= fromElement
    justThen f el

justThen :: ∀ a. (a -> Effect Unit) -> (Maybe a) -> Effect Unit
justThen f a =
    if isJust a then f (unsafePartial fromJust a) else pure unit

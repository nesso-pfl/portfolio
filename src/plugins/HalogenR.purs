module Plugin.HalogenR 
    ( divC
    , divCE
    , divC1
    , spanC1
    , h1C1
    , h2C1
    , h2C
    , divCE1
    , divCI1
    , inputtextIE
    , textareaIE
    , buttonCE1
    , withDomId
    , textForm
    )
    where

import Prelude
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect (Effect)
import Halogen.HTML (HTML, ClassName(..), div, text, button, label, input, small, span, h1, h2, textarea)
import Halogen.HTML.Properties (class_, id_, value, for, placeholder)
import Halogen.HTML.Core (PropName(..))
import Halogen.HTML.Events (onClick, onKeyDown, onInput, onValueInput, onValueChange)
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

divCE :: ∀ p i. String -> i -> Array (HTML p i) -> HTML p i
divCE cls action es =
    div [ class_ $ ClassName cls, onClick \_ -> Just action ] es

divC1 :: ∀ p i. String -> HTML p i -> HTML p i
divC1 cls e = div [ class_ $ ClassName cls ] [ e ]

spanC1 :: ∀ p i. String -> HTML p i -> HTML p i
spanC1 cls e = span [ class_ $ ClassName cls ] [ e ]

h1C1 :: ∀ p i. String -> HTML p i -> HTML p i
h1C1 cls e = h1 [ class_ $ ClassName cls ] [ e ]

h2C1 :: ∀ p i. String -> HTML p i -> HTML p i
h2C1 cls e = h2 [ class_ $ ClassName cls ] [ e ]

h2C :: ∀ p i. String -> Array (HTML p i) -> HTML p i
h2C cls es = h2 [ class_ $ ClassName cls ] es

divC2 :: ∀ p i. String -> HTML p i -> HTML p i -> HTML p i
divC2 cls e1 e2 = div [ class_ $ ClassName cls ] [ e1, e2 ]

divCE1 :: ∀ p i. String -> i -> HTML p i -> HTML p i
divCE1 cls action e =
    div [ class_ $ ClassName cls, onClick \_ -> Just action ] [ e ]

labelC1 :: ∀ p i. String -> String -> HTML p i -> HTML p i
labelC1 cls label_ e =
    label [ class_ $ ClassName cls, for label_ ] [ e ]

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

textForm :: ∀ p i. String -> String -> String -> (String -> i) -> HTML p i
textForm cls label_ placeholder_ action = divC (cls <> " text-form")
    [ labelC1 "pretext" label_ $ text (label_ <> ": ")
    , input [ id_ label_, placeholder placeholder_, onValueChange $ Just <<< action ]
    ]

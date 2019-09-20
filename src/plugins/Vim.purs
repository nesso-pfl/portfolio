module Plugin.Vim (addKeydownEvent) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Maybe (Maybe, fromJust, isJust)
import Effect (Effect)
import Effect.Console (log)
import Effect.Uncurried (EffectFn1, runEffectFn1, runEffectFn3)
import Foreign (Foreign)
import Foreign.Class (class Decode, decode)
import Foreign.Generic (defaultOptions, genericDecode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Halogen.VDom.Util (addEventListener)
import Partial.Unsafe (unsafePartial)
import Web.Event.Event (Event, preventDefault)
import Web.Event.EventTarget (eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument, toNonElementParentNode)
import Web.HTML.HTMLElement (HTMLElement, blur, focus, fromElement)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key, ctrlKey, shiftKey, fromEvent, toEvent)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.Document (documentElement)


addKeydownEvent :: Effect Unit
addKeydownEvent = do
    el <- window >>= document >>= toDocument >>> documentElement
    el # justThen \e -> do
        listener <- eventListener keyEvent
        runEffectFn3 addEventListener "keydown" listener e

keyEvent :: Event -> Effect Unit
keyEvent e = fromEvent e # justThen keyEvent_

keyEvent_ :: KeyboardEvent -> Effect Unit
keyEvent_ e | ctrlKey e = pure unit
keyEvent_ e | shiftKey e = pure unit
keyEvent_ e | otherwise = case key e of
    "i" -> do
        preventDefault $ toEvent e
        withVimDom focus
    "Escape" -> do
        withVimDom blur
    _ -> do
        log $ key e
        pure unit

tokenize' :: String -> Effect String
tokenize' s = do
    dict <- runEffectFn1 tokenize_ s
    either (pure <<< show) pure (runExcept $ decode dict)
foreign import tokenize_ :: EffectFn1 String Foreign

withVimDom :: (HTMLElement -> Effect Unit) -> Effect Unit
withVimDom f = do
    dom <- window >>= document >>= toNonElementParentNode >>> getElementById "editor-vim"
    let el = dom >>= fromElement
    justThen f el

justThen :: forall a. (a -> Effect Unit) -> (Maybe a) -> Effect Unit
justThen f a =
    if isJust a then f (unsafePartial fromJust a) else pure unit

newtype Word = Word {
    surface_form :: String
}

type Words = Array Word

instance docodeWord :: Decode Word where
    decode = genericDecode defaultOptions

derive instance genericWord :: Generic Word _

instance showWord :: Show Word where
    show = genericShow

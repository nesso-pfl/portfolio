module Main where

import Router as R

import Prelude
import Control.Coroutine as CR
import Data.Maybe (Maybe(..))
import Foreign (unsafeToForeign)
import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.PushState as RP


main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    psi <- liftEffect RP.makeInterface
    io <- runUI R.ui unit body
    _ <- liftEffect $ RP.matches R.routing (R.listenRoute io) psi
    io.subscribe $ CR.consumer \url -> liftEffect do
        psi.pushState (unsafeToForeign {}) url
        pure Nothing

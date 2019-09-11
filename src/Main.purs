module Main where

import Router as R

import Prelude
import Foreign (unsafeToForeign)
import Effect (Effect)
import Effect.Console (log)
import Effect.Aff (forkAff)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.PushState as RP


main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    -- runUI R.ui unit body
    psi <- liftEffect RP.makeInterface
    ls <- liftEffect psi.locationState
    driver <- runUI R.ui ls.path body
    _ <- liftEffect $ psi.listen (R.listen driver)
    liftEffect $ log ls.path
    -- forkAff $ R.routeSignal driver

module Main where

import Router as R

import Prelude
import Effect (Effect)
import Effect.Aff (forkAff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    driver <- runUI R.ui unit body
    forkAff $ R.routeSignal driver

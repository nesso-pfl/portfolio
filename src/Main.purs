module Main where

import Router as Router

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    runUI Router.ui unit body

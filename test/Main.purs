module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT, defaultConfig)
import Tests as Tests

main :: Effect Unit
main =
  launchAff_
    $ do
        let
          specConfig = defaultConfig { timeout = Just (Milliseconds $ 120.0 * 1000.0) }
        join
          $ runSpecT specConfig [ consoleReporter ] do
            Tests.spec



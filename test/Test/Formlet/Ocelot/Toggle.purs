module Test.Formlet.Ocelot.Toggle
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Identity as Data.Identity
import Formlet as Formlet
import Formlet.Ocelot.Toggle as Formlet.Ocelot.Toggle
import Formlet.Render as Formlet.Render
import Test.QuickCheck ((===))
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Formlet.Ocelot.Toggle" do
    Test.Unit.test "`toggle` should not change its value if `readonly = true`" do
      Test.Unit.QuickCheck.quickCheck \value value' readonly ->
        let
          rendered :: Formlet.Ocelot.Toggle.Render (Boolean -> Boolean)
          rendered =
            Formlet.Render.match { toggle: map (un Data.Identity.Identity) }
              $ Formlet.render (Formlet.Ocelot.Toggle.toggle {}) { readonly }
              $ value

          expected :: Boolean
          expected = if readonly then value else value'
        in
          expected === (un Formlet.Ocelot.Toggle.Render rendered).onChange value' value

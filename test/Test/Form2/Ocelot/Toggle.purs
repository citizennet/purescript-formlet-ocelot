module Test.Form2.Ocelot.Toggle
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Identity as Data.Identity
import Form2 as Form2
import Form2.Ocelot.Toggle as Form2.Ocelot.Toggle
import Form2.Render as Form2.Render
import Test.QuickCheck ((===))
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Form2.Ocelot.Toggle" do
    Test.Unit.test "`toggle` should not change its value if `readonly = true`" do
      Test.Unit.QuickCheck.quickCheck \value value' readonly ->
        let
          rendered :: Form2.Ocelot.Toggle.Render (Boolean -> Boolean)
          rendered =
            Form2.Render.match { toggle: map (un Data.Identity.Identity) }
              $ Form2.render Form2.Ocelot.Toggle.toggle { readonly }
              $ value

          expected :: Boolean
          expected = if readonly then value else value'
        in
          expected === (un Form2.Ocelot.Toggle.Render rendered).onChange value' value

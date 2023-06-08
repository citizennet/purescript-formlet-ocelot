module Test.Formlet.Ocelot.Text
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Identity as Data.Identity
import Formlet as Formlet
import Formlet.Ocelot.Text as Formlet.Ocelot.Text
import Formlet.Render as Formlet.Render
import Test.QuickCheck ((===))
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Formlet.Ocelot.Text" do
    Test.Unit.test "`text` should not change its value if `readonly = true`" do
      Test.Unit.QuickCheck.quickCheck \value value' readonly ->
        let
          rendered :: Formlet.Ocelot.Text.Render (String -> String)
          rendered =
            Formlet.Render.match { text: map (un Data.Identity.Identity) }
              $ Formlet.render
                  ( Formlet.Ocelot.Text.text
                      { addonLeft: Nothing
                      , placeholder: ""
                      }
                  )
                  { readonly }
              $ value

          expected :: String
          expected = if readonly then value else value'
        in
          expected === (un Formlet.Ocelot.Text.Render rendered).onChange value' value

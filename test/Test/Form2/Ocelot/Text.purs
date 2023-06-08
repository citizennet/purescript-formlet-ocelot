module Test.Form2.Ocelot.Text
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Identity as Data.Identity
import Form2 as Form2
import Form2.Ocelot.Text as Form2.Ocelot.Text
import Form2.Render as Form2.Render
import Test.QuickCheck ((===))
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Form2.Ocelot.Text" do
    Test.Unit.test "`text` should not change its value if `readonly = true`" do
      Test.Unit.QuickCheck.quickCheck \value value' readonly ->
        let
          rendered :: Form2.Ocelot.Text.Render (String -> String)
          rendered =
            Form2.Render.match { text: map (un Data.Identity.Identity) }
              $ Form2.render
                  ( Form2.Ocelot.Text.text
                      { addonLeft: Nothing
                      , placeholder: ""
                      }
                  )
                  { readonly }
              $ value

          expected :: String
          expected = if readonly then value else value'
        in
          expected === (un Form2.Ocelot.Text.Render rendered).onChange value' value

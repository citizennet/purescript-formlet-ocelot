module Test.Form2.Ocelot.Textarea
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Identity as Data.Identity
import Form2 as Form2
import Form2.Ocelot.Textarea as Form2.Ocelot.Textarea
import Form2.Render as Form2.Render
import Test.QuickCheck ((===))
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Form2.Ocelot.Textarea" do
    Test.Unit.test "`textarea` should not change its value if `readonly = true`" do
      Test.Unit.QuickCheck.quickCheck \value value' readonly ->
        let
          rendered :: Form2.Ocelot.Textarea.Render (String -> String)
          rendered =
            Form2.Render.match { textarea: map (un Data.Identity.Identity) }
              $ Form2.render (Form2.Ocelot.Textarea.textarea { placeholder: "", rows: 3 }) { readonly }
              $ value

          expected :: String
          expected = if readonly then value else value'
        in
          expected === (un Form2.Ocelot.Textarea.Render rendered).onChange value' value

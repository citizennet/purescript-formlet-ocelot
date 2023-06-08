module Test.Formlet.Ocelot.Textarea
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Identity as Data.Identity
import Formlet as Formlet
import Formlet.Ocelot.Textarea as Formlet.Ocelot.Textarea
import Formlet.Render as Formlet.Render
import Test.QuickCheck ((===))
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Formlet.Ocelot.Textarea" do
    Test.Unit.test "`textarea` should not change its value if `readonly = true`" do
      Test.Unit.QuickCheck.quickCheck \value value' readonly ->
        let
          rendered :: Formlet.Ocelot.Textarea.Render (String -> String)
          rendered =
            Formlet.Render.match { textarea: map (un Data.Identity.Identity) }
              $ Formlet.render (Formlet.Ocelot.Textarea.textarea { placeholder: "", rows: 3 }) { readonly }
              $ value

          expected :: String
          expected = if readonly then value else value'
        in
          expected === (un Formlet.Ocelot.Textarea.Render rendered).onChange value' value

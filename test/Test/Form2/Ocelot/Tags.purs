module Test.Form2.Ocelot.Tags
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Array.NonEmpty as Data.Array.NonEmpty
import Data.Identity as Data.Identity
import Form2 as Form2
import Form2.Ocelot.Tags as Form2.Ocelot.Tags
import Form2.Render as Form2.Render
import Test.QuickCheck ((===))
import Test.QuickCheck.Gen as Test.QuickCheck.Gen
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Form2.Ocelot.Tags" do
    Test.Unit.test "`tags` should not change its value if `readonly = true`" do
      Test.Unit.QuickCheck.quickCheck \items readonly -> do
        value <- Test.QuickCheck.Gen.arrayOf (Test.QuickCheck.Gen.elements items)
        value' <- Test.QuickCheck.Gen.arrayOf (Test.QuickCheck.Gen.elements items)
        let
          rendered :: Form2.Ocelot.Tags.Render (Array String -> Array String)
          rendered =
            Form2.Render.match { tags: map (un Data.Identity.Identity) }
              $ Form2.render
                  ( Form2.Ocelot.Tags.tags
                      { items: Data.Array.NonEmpty.toArray items
                      , placeholder:
                          { primary: ""
                          , secondary: ""
                          }
                      }
                  )
                  { readonly }
                  value

          expected :: Array String
          expected = if readonly then value else value'
        pure $ expected === (un Form2.Ocelot.Tags.Render rendered).onChange value' value

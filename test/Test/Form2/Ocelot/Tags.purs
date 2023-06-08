module Test.Formlet.Ocelot.Tags
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Array.NonEmpty as Data.Array.NonEmpty
import Data.Identity as Data.Identity
import Formlet as Formlet
import Formlet.Ocelot.Tags as Formlet.Ocelot.Tags
import Formlet.Render as Formlet.Render
import Test.QuickCheck ((===))
import Test.QuickCheck.Gen as Test.QuickCheck.Gen
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Formlet.Ocelot.Tags" do
    Test.Unit.test "`tags` should not change its value if `readonly = true`" do
      Test.Unit.QuickCheck.quickCheck \items readonly -> do
        value <- Test.QuickCheck.Gen.arrayOf (Test.QuickCheck.Gen.elements items)
        value' <- Test.QuickCheck.Gen.arrayOf (Test.QuickCheck.Gen.elements items)
        let
          rendered :: Formlet.Ocelot.Tags.Render (Array String -> Array String)
          rendered =
            Formlet.Render.match { tags: map (un Data.Identity.Identity) }
              $ Formlet.render
                  ( Formlet.Ocelot.Tags.tags
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
        pure $ expected === (un Formlet.Ocelot.Tags.Render rendered).onChange value' value

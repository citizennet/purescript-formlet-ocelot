module Test.Formlet.Ocelot.Checkbox
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Data.Array.NonEmpty as Data.Array.NonEmpty
import Data.Identity as Data.Identity
import Data.Maybe as Data.Maybe
import Formlet as Formlet
import Formlet.Ocelot.Checkbox as Formlet.Ocelot.Checkbox
import Formlet.Render as Formlet.Render
import Partial.Unsafe as Partial.Unsafe
import Test.QuickCheck ((===))
import Test.QuickCheck as Test.QuickCheck
import Test.QuickCheck.Gen as Test.QuickCheck.Gen
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Formlet.Ocelot.Checkbox" do
    checkboxSuite

checkboxSuite :: Test.Unit.TestSuite
checkboxSuite = do
  Test.Unit.suite "`checkbox`" do
    Test.Unit.test "`checkbox` should not change its value if `readonly = true`" do
      Test.Unit.QuickCheck.quickCheck \value value' readonly ->
        let
          rendered :: Formlet.Ocelot.Checkbox.Render (Boolean -> Boolean)
          rendered =
            map (un Data.Identity.Identity)
              $ Formlet.render (Formlet.Ocelot.Checkbox.checkbox "Foo") { readonly }
              $ value

          expected :: Boolean
          expected = if readonly then value else value'

          onChange' :: Maybe (Boolean -> Boolean -> Boolean)
          onChange' = ado
            option <- Data.Array.head (un Formlet.Ocelot.Checkbox.Render rendered)
            in option.onChange
        in
          case onChange' of
            Nothing -> Test.QuickCheck.Failed "Empty options Array"
            Just onChange -> expected === onChange value' value
  Test.Unit.suite "`checkboxes`" do
    Test.Unit.test "`checkboxes` should not change any of its values if `readonly = true`" do
      Test.Unit.QuickCheck.quickCheck \value checked readonly -> do
        let
          rendered :: Formlet.Ocelot.Checkbox.Render ({ foo :: Boolean, bar :: Boolean } -> { foo :: Boolean, bar :: Boolean })
          rendered =
            Formlet.Render.match { checkbox: map (un Data.Identity.Identity) }
              $ Formlet.render
                  ( Formlet.Ocelot.Checkbox.checkboxes ado
                      foo <- Formlet.overRecord { foo: _ } (Formlet.Ocelot.Checkbox.checkbox "Foo")
                      bar <- Formlet.overRecord { bar: _ } (Formlet.Ocelot.Checkbox.checkbox "Bar")
                      in { foo, bar }
                  )
                  { readonly }
                  value

          options ::
            NonEmptyArray
              { checked :: Boolean
              , label :: String
              , onChange :: Boolean -> { foo :: Boolean, bar :: Boolean } -> { foo :: Boolean, bar :: Boolean }
              , readonly :: Boolean
              }
          options =
            -- We know here that `options` is a `NonEmptyArray` and we need to
            -- make the set of rendered options into one in order to use
            -- `Test.QuickCheck.Gen.elements`.
            Partial.Unsafe.unsafePartial
              $ Data.Maybe.fromJust
              $ Data.Array.NonEmpty.fromArray
              $ un Formlet.Ocelot.Checkbox.Render rendered
        selectedOption <- Test.QuickCheck.Gen.elements options
        let
          expected :: { foo :: Boolean, bar :: Boolean }
          expected =
            if readonly then
              value
            else case selectedOption.label of
              "Foo" -> value { foo = checked }
              "Bar" -> value { bar = checked }
              _ -> value
        pure $ expected === selectedOption.onChange checked value

module Test.Formlet.Ocelot.Radio
  ( suite
  ) where

import CitizenNet.Prelude

import Control.Monad.Gen.Common as Control.Monad.Gen.Common
import Data.Array as Data.Array
import Data.Array.NonEmpty as Data.Array.NonEmpty
import Data.Identity as Data.Identity
import Data.Maybe as Data.Maybe
import Formlet as Formlet
import Formlet.Ocelot.Radio as Formlet.Ocelot.Radio
import Formlet.Render as Formlet.Render
import Partial.Unsafe as Partial.Unsafe
import Test.QuickCheck ((===))
import Test.QuickCheck.Gen as Test.QuickCheck.Gen
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Formlet.Ocelot.Radio" do
    Test.Unit.test "`radio` should render all options" do
      Test.Unit.QuickCheck.quickCheck \options ->
        let
          rendered :: Formlet.Ocelot.Radio.Render (Maybe String -> Maybe String)
          rendered =
            Formlet.Render.match { radio: map (un Data.Identity.Identity) }
              $ Formlet.render
                  ( Formlet.Ocelot.Radio.radio
                      { display: (_ <> "a")
                      , options
                      }
                  )
                  { readonly: false }
                  Nothing

          expected :: Array String
          expected = map (_ <> "a") options
        in
          expected === map _.label (un Formlet.Ocelot.Radio.Render rendered).options
    Test.Unit.test "A `radio` option's `onSelect` should set the value to that option" do
      Test.Unit.QuickCheck.quickCheck \options -> do
        value <- Control.Monad.Gen.Common.genMaybe (Test.QuickCheck.Gen.elements options)
        let
          rendered :: NonEmptyArray { label :: String, onSelect :: Maybe String -> Maybe String }
          rendered =
            testRenderRadioOptions
              { display: identity
              , options
              , readonly: false
              , value
              }
        selectedOption <- Test.QuickCheck.Gen.elements rendered
        pure $ Just selectedOption.label === selectedOption.onSelect value
    Test.Unit.test "`radio` should appear to have no rendered value if the selected value is not in the options" do
      Test.Unit.QuickCheck.quickCheck \value options ->
        let
          rendered :: Formlet.Ocelot.Radio.Render (Maybe String -> Maybe String)
          rendered =
            Formlet.Render.match { radio: map (un Data.Identity.Identity) }
              $ Formlet.render
                  ( Formlet.Ocelot.Radio.radio
                      { display: identity
                      , options
                      }
                  )
                  { readonly: false }
                  value

          expected :: Maybe String
          expected = case value of
            Nothing -> Nothing
            Just value'
              | Data.Array.elem value' options -> Just value'
            Just _ -> Nothing
        in
          expected === (un Formlet.Ocelot.Radio.Render rendered).value
    Test.Unit.test "`radio` should validate as having no selected value if the selected value is not in the options" do
      Test.Unit.QuickCheck.quickCheck \value options ->
        let
          result :: Either (Array String) (Maybe String)
          result =
            Formlet.validate
              ( Formlet.Ocelot.Radio.radio
                  { display: identity
                  , options
                  } ::
                  Formlet.Form _ _ Data.Identity.Identity _ _
              )
              { readonly: false }
              value

          expected :: Either (Array String) (Maybe String)
          expected = case value of
            Nothing -> Right Nothing
            Just value'
              | Data.Array.elem value' options -> Right (Just value')
            Just _ -> Right Nothing
        in
          expected === result
    Test.Unit.test "`radio` should not change its value if `readonly = true`" do
      Test.Unit.QuickCheck.quickCheck \options readonly -> do
        value <- Control.Monad.Gen.Common.genMaybe (Test.QuickCheck.Gen.elements options)
        let
          rendered :: NonEmptyArray { label :: String, onSelect :: Maybe String -> Maybe String }
          rendered =
            testRenderRadioOptions
              { display: identity
              , options
              , readonly
              , value
              }
        selectedOption <- Test.QuickCheck.Gen.elements rendered
        let
          expected :: Maybe String
          expected = if readonly then value else Just selectedOption.label
        pure $ expected === selectedOption.onSelect value

testRenderRadioOptions ::
  forall a.
  Eq a =>
  { display :: a -> String
  , options :: NonEmptyArray a
  , readonly :: Boolean
  , value :: Maybe a
  } ->
  NonEmptyArray { label :: String, onSelect :: Maybe a -> Maybe a }
testRenderRadioOptions { display, options, readonly, value } =
  -- We know here that the `options` is a `NonEmptyArray` and we need to make
  -- the set of rendered options into one in order to use
  -- `Test.QuickCheck.Gen.elements`.
  Partial.Unsafe.unsafePartial
    $ Data.Maybe.fromJust
    $ Data.Array.NonEmpty.fromArray
    $ Formlet.Render.match { radio: _.options <<< un Formlet.Ocelot.Radio.Render <<< map (un Data.Identity.Identity) }
    $ Formlet.render
        ( Formlet.Ocelot.Radio.radio
            { display
            , options: Data.Array.NonEmpty.toArray options
            }
        )
        { readonly }
        value

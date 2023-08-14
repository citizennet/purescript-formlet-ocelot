module Test.Formlet.Ocelot.CheckboxSet
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Data.Array.NonEmpty as Data.Array.NonEmpty
import Data.Identity as Data.Identity
import Data.Maybe as Data.Maybe
import Data.Ord as Data.Ord
import Data.Set as Data.Set
import Formlet as Formlet
import Formlet.Render as Formlet.Render
import Formlet.Ocelot.CheckboxSet as Formlet.Ocelot.CheckboxSet
import Partial.Unsafe as Partial.Unsafe
import Test.QuickCheck ((===))
import Test.QuickCheck.Gen as Test.QuickCheck.Gen
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Formlet.Ocelot.CheckboxSet" do
    checkboxSetSuite

checkboxSetSuite :: Test.Unit.TestSuite
checkboxSetSuite =
  Test.Unit.suite "`checkboxSet`" do
    Test.Unit.test "`checkboxSet` should render all options" do
      Test.Unit.QuickCheck.quickCheck \options ->
        let
          rendered :: Formlet.Ocelot.CheckboxSet.Render (Set String -> Set String)
          rendered =
            Formlet.Render.match { checkboxSet: map (un Data.Identity.Identity) }
              $ Formlet.render
                  ( Formlet.Ocelot.CheckboxSet.checkboxSet
                      { display: (_ <> "a")
                      , options
                      }
                  )
                  { readonly: false }
                  Data.Set.empty

          expected :: Array String
          expected = map (_ <> "a") options
        in
          expected === map _.label (un Formlet.Ocelot.CheckboxSet.Render rendered).options
    Test.Unit.test "A `checkboxSet` option's `onChange` should appropriately change the Form's value" do
      Test.Unit.QuickCheck.quickCheck \options checked -> do
        value <- Data.Set.fromFoldable <$> genTake (Data.Array.NonEmpty.toArray options)
        let
          rendered ::
            NonEmptyArray
              { checked :: Boolean
              , label :: String
              , onChange :: Boolean -> Set String -> Set String
              }
          rendered =
            testRenderCheckboxSetOptions
              { display: identity
              , options
              , readonly: false
              , value
              }
        selectedOption <- Test.QuickCheck.Gen.elements rendered
        let
          expected :: Set String
          expected =
            if checked then
              Data.Set.insert selectedOption.label value
            else
              Data.Set.delete selectedOption.label value
        pure $ expected === selectedOption.onChange checked value
    Test.Unit.test "`checkboxSet`'s selected values which are not in the options should not appear in the rendered value" do
      Test.Unit.QuickCheck.quickCheck \(value' :: Array String) options ->
        let
          value :: Set String
          value = Data.Set.fromFoldable value'

          rendered ::
            NonEmptyArray
              { checked :: Boolean
              , label :: String
              , onChange :: Boolean -> Set String -> Set String
              }
          rendered =
            testRenderCheckboxSetOptions
              { display: identity
              , options
              , readonly: false
              , value
              }

          -- Here we choose to test whether no values in the Form `value` that
          -- do not exist in the `options` end up appearing in the rendered
          -- `options`, instead of mapping over the `options`, as we already
          -- know from a previous test that `checkboxSet` renders only the
          -- `options`.
          expected :: Array { checked :: Boolean, label :: String }
          expected =
            Data.Array.sortWith _.label $ value
              # foldMap \label ->
                  if Data.Array.NonEmpty.elem label options then
                    [ { checked: true, label } ]
                  else
                    []

          actual :: Array { checked :: Boolean, label :: String }
          actual =
            Data.Array.nubBy (Data.Ord.comparing _.label)
              $ Data.Array.sortWith _.label
              $ rendered
              # foldMap \{ checked, label } ->
                  if checked then
                    [ { checked, label } ]
                  else
                    []
        in
          expected === actual
    Test.Unit.test "`checkboxSet`'s validated result should not include any values that are not in the options" do
      Test.Unit.QuickCheck.quickCheck \(value' :: Array String) options ->
        let
          value :: Set String
          value = Data.Set.fromFoldable value'

          result :: Either (Array String) (Set String)
          result =
            Formlet.validate
              ( Formlet.Ocelot.CheckboxSet.checkboxSet
                  { display: identity :: String -> String
                  , options
                  } ::
                  Formlet.Form _ _ Data.Identity.Identity _ _
              )
              { readonly: false }
              value

          expected :: Either (Array String) (Set String)
          expected = Right $ Data.Set.intersection value (Data.Set.fromFoldable options)
        in
          expected === result
    Test.Unit.test "`checkboxSet` should not change its value if `readonly = true`" do
      Test.Unit.QuickCheck.quickCheck \options checked readonly -> do
        value <- Data.Set.fromFoldable <$> genTake (Data.Array.NonEmpty.toArray options)
        let
          rendered ::
            NonEmptyArray
              { checked :: Boolean
              , label :: String
              , onChange :: Boolean -> Set String -> Set String
              }
          rendered =
            testRenderCheckboxSetOptions
              { display: identity
              , options
              , readonly
              , value
              }
        selectedOption <- Test.QuickCheck.Gen.elements rendered 
        let
          expected :: Set String
          expected = case readonly, checked of
            true, true -> value
            true, false -> value
            false, true -> Data.Set.insert selectedOption.label value
            false, false -> Data.Set.delete selectedOption.label value
        pure $ expected === selectedOption.onChange checked value

genTake :: forall a. Array a -> Test.QuickCheck.Gen.Gen (Array a)
genTake xs = do
  n <- Test.QuickCheck.Gen.chooseInt 0 (Data.Array.length xs - 1)
  Data.Array.take n <$> Test.QuickCheck.Gen.shuffle (xs)

testRenderCheckboxSetOptions ::
  forall a.
  Ord a =>
  { display :: a -> String
  , options :: NonEmptyArray a
  , readonly :: Boolean
  , value :: Set a
  } ->
  NonEmptyArray
    { checked :: Boolean
    , label :: String
    , onChange :: Boolean -> Set a -> Set a
    }
testRenderCheckboxSetOptions { display, options, readonly, value } =
  -- We know here that the `options` is a `NonEmptyArray` and we need to make
  -- the set of rendered options into one in order to use
  -- `Test.QuickCheck.Gen.elements`.
  Partial.Unsafe.unsafePartial
    $ Data.Maybe.fromJust
    $ Data.Array.NonEmpty.fromArray
    $ _.options
    $ un Formlet.Ocelot.CheckboxSet.Render
    $ Formlet.Render.match { checkboxSet: map (un Data.Identity.Identity) }
    $ Formlet.render
        ( Formlet.Ocelot.CheckboxSet.checkboxSet
            { display
            , options: Data.Array.NonEmpty.toArray options
            }
        )
        { readonly }
        value

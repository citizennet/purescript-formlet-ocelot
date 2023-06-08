module Test.Formlet.Ocelot.Typeahead
  ( suite
  ) where

import CitizenNet.Prelude

import Control.Monad.Gen.Common as Control.Monad.Gen.Common
import Data.Array as Data.Array
import Data.Array.NonEmpty as Data.Array.NonEmpty
import Data.Identity as Data.Identity
import Formlet as Formlet
import Formlet.Ocelot.Typeahead as Formlet.Ocelot.Typeahead
import Test.QuickCheck ((===))
import Test.QuickCheck.Arbitrary as Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen as Test.QuickCheck.Gen
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Formlet.Ocelot.Typeahead" do
    Test.Unit.suite "`sync` typeahead" do
      Test.Unit.test "`sync` typeahead should not change to a value that is not in the options" do
        Test.Unit.QuickCheck.quickCheck \value isInOptions items -> do
          value' <-
            if isInOptions then
              Control.Monad.Gen.Common.genMaybe (Test.QuickCheck.Gen.elements items)
            else
              Test.QuickCheck.Arbitrary.arbitrary
          let
            rendered :: Formlet.Ocelot.Typeahead.Render Maybe String (Maybe String -> Maybe String)
            rendered =
              map (un Data.Identity.Identity)
                $ Formlet.render
                    ( Formlet.Ocelot.Typeahead.sync
                        { items: pure (Data.Array.NonEmpty.toArray items)
                        , toSearchRecord
                        }
                    )
                    { readonly: false }
                    value

            expected :: Maybe String
            expected = case value' of
              Nothing -> Nothing
              Just value''
                | Data.Array.NonEmpty.elem value'' items -> Just value''
              Just _ -> value
          pure $ expected === (un Formlet.Ocelot.Typeahead.Render rendered).onChange value' value
      Test.Unit.test "`sync` typeahead should appear to have no rendered value if the selected value is not in the options" do
        Test.Unit.QuickCheck.quickCheck \value items ->
          let
            rendered :: Formlet.Ocelot.Typeahead.Render Maybe String (Maybe String -> Maybe String)
            rendered =
              map (un Data.Identity.Identity)
                $ Formlet.render (Formlet.Ocelot.Typeahead.sync { items: pure items, toSearchRecord }) { readonly: false }
                $ value

            expected :: Maybe String
            expected = case value of
              Nothing -> Nothing
              Just value'
                | Data.Array.elem value' items -> Just value'
              Just _ -> Nothing
          in
            expected === (un Formlet.Ocelot.Typeahead.Render rendered).value
      Test.Unit.test "`sync` typeahead should validate as having no selected value if the selected value is not in the options" do
        Test.Unit.QuickCheck.quickCheck \value items ->
          let
            result :: Either (Array String) (Maybe String)
            result =
              Formlet.validate
                (Formlet.Ocelot.Typeahead.sync { items: pure items, toSearchRecord } :: Formlet.Form _ _ Data.Identity.Identity _ _)
                { readonly: false }
                value

            expected :: Either (Array String) (Maybe String)
            expected = case value of
              Nothing -> Right Nothing
              Just value'
                | Data.Array.elem value' items -> Right (Just value')
              Just _ -> Right Nothing
          in
            expected === result
      Test.Unit.test "`sync` typeahead should render its value as the first available option that satisfies the `select` predicate" do
        Test.Unit.QuickCheck.quickCheck \items -> do
          value <- Control.Monad.Gen.Common.genMaybe (Test.QuickCheck.Gen.elements items)
          let
            select :: { id :: Int, value :: String } -> { id :: Int, value :: String } -> Boolean
            select a b = a.id == b.id

            rendered :: Formlet.Ocelot.Typeahead.Render Maybe { id :: Int, value :: String } (Maybe { id :: Int, value :: String } -> Maybe { id :: Int, value :: String })
            rendered =
              map (un Data.Identity.Identity)
                $ Formlet.render
                    ( Formlet.Ocelot.Typeahead.sync
                        { items: pure (Data.Array.NonEmpty.toArray items)
                        , select
                        , toSearchRecord: toSearchRecord <<< _.value
                        }
                    )
                    { readonly: false }
                    (map _ { value = "" } value)

            expected :: Maybe { id :: Int, value :: String }
            expected = case value of
              Nothing -> Nothing
              Just value' -> Data.Array.NonEmpty.find (select value') items
          pure $ expected === (un Formlet.Ocelot.Typeahead.Render rendered).value
      Test.Unit.test "`sync` typeahead should not change its value if `readonly = true`" do
        Test.Unit.QuickCheck.quickCheck \items readonly -> do
          value <- Control.Monad.Gen.Common.genMaybe (Test.QuickCheck.Gen.elements items)
          value' <- Control.Monad.Gen.Common.genMaybe (Test.QuickCheck.Gen.elements items)
          let
            rendered :: Formlet.Ocelot.Typeahead.Render Maybe String (Maybe String -> Maybe String)
            rendered =
              map (un Data.Identity.Identity)
                $ Formlet.render
                    ( Formlet.Ocelot.Typeahead.sync
                        { items: pure (Data.Array.NonEmpty.toArray items)
                        , toSearchRecord
                        }
                    )
                    { readonly }
                    value

            expected :: Maybe String
            expected = if readonly then value else value'
          pure $ expected === (un Formlet.Ocelot.Typeahead.Render rendered).onChange value' value
    -- For async typeaheads we cannot test validation or the interaction between
    -- the selected value and the item options because the options are loaded
    -- asynchronously. For that reason, validation should also be asynchronous
    -- and effectful, as the external source that provides the options may
    -- change at any time between the selection and the validation.
    Test.Unit.suite "`async` typeahead" do
      Test.Unit.test "`async` typeahead should not change its value if `readonly = true`" do
        Test.Unit.QuickCheck.quickCheck \items readonly -> do
          value <- Control.Monad.Gen.Common.genMaybe (Test.QuickCheck.Gen.elements items)
          value' <- Control.Monad.Gen.Common.genMaybe (Test.QuickCheck.Gen.elements items)
          let
            search :: String -> Aff (Either String (Array String))
            search _ = pure (Right (Data.Array.NonEmpty.toArray items))

            rendered :: Formlet.Ocelot.Typeahead.Render Maybe String (Maybe String -> Maybe String)
            rendered =
              map (un Data.Identity.Identity)
                $ Formlet.render (Formlet.Ocelot.Typeahead.async { search, toSearchRecord }) { readonly }
                $ value

            expected :: Maybe String
            expected = if readonly then value else value'
          pure $ expected === (un Formlet.Ocelot.Typeahead.Render rendered).onChange value' value

toSearchRecord :: String -> { item :: String }
toSearchRecord = { item: _ }

module Test.Form2.Ocelot.Dropdown
  ( suite
  ) where

import CitizenNet.Prelude

import Control.Monad.Gen.Common as Control.Monad.Gen.Common
import Data.Array as Data.Array
import Data.Array.NonEmpty as Data.Array.NonEmpty
import Data.Identity as Data.Identity
import Data.Maybe as Data.Maybe
import Form2 as Form2
import Form2.Ocelot.Dropdown as Form2.Ocelot.Dropdown
import Form2.Render as Form2.Render
import Partial.Unsafe as Partial.Unsafe
import Test.QuickCheck ((===))
import Test.QuickCheck.Gen as Test.QuickCheck.Gen
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Form2.Ocelot.Dropdown" do
    Test.Unit.test "`dropdown` should render all options" do
      -- We ignore the first option in this test as we verify it in another test
      Test.Unit.QuickCheck.quickCheck \options ->
        let
          rendered :: Form2.Ocelot.Dropdown.Render (Maybe String -> Maybe String)
          rendered =
            Form2.Render.match { dropdown: map (un Data.Identity.Identity) }
              $ Form2.render
                  ( Form2.Ocelot.Dropdown.dropdown
                      { display: (_ <> "a")
                      , options
                      , placeholder: "placeholder"
                      }
                  )
                  { readonly: false }
                  Nothing

          expected :: Array String
          expected = map (_ <> "a") options
        in
          Form2.Ocelot.Dropdown.withRender rendered \render ->
            expected === map render.display (fromMaybe [] (Data.Array.tail render.options))
    Test.Unit.test "`dropdown` should render the first option as the placeholder if no option is selected, or as an empty string otherwise" do
      Test.Unit.QuickCheck.quickCheck \options' placeholder -> ado
        value <- Control.Monad.Gen.Common.genMaybe (Test.QuickCheck.Gen.elements options')
        let
          options :: Array String
          options = Data.Array.NonEmpty.toArray options'

          rendered :: Form2.Ocelot.Dropdown.Render (Maybe String -> Maybe String)
          rendered =
            Form2.Render.match { dropdown: map (un Data.Identity.Identity) }
              $ Form2.render
                  ( Form2.Ocelot.Dropdown.dropdown
                      { display: identity
                      , options
                      , placeholder
                      }
                  )
                  { readonly: false }
                  value

          expected :: Array String
          expected =
            case value of
              Nothing -> [ placeholder ] <> options
              Just _ -> [ "" ] <> options
        in
          Form2.Ocelot.Dropdown.withRender rendered \render ->
            expected === map render.display render.options
    Test.Unit.test "`dropdown` should appear to have no rendered value if the selected value is not in the options" do
      Test.Unit.QuickCheck.quickCheck \value options ->
        let
          rendered :: Form2.Ocelot.Dropdown.Render (Maybe String -> Maybe String)
          rendered =
            Form2.Render.match { dropdown: map (un Data.Identity.Identity) }
              $ Form2.render
                  ( Form2.Ocelot.Dropdown.dropdown
                      { display: identity
                      , options
                      , placeholder: ""
                      }
                  )
                  { readonly: false }
                  value
        in
          Form2.Ocelot.Dropdown.withRender rendered \render ->
            let
              expected :: Maybe String
              expected = case value of
                Nothing -> Nothing
                Just value'
                  | Data.Array.elem value' options -> Just value'
                Just _ -> Nothing
            in
              expected === map (render.display <<< Just) render.value
    Test.Unit.test "`dropdown` should validate as having no selected value if the selected value is not in the options" do
      Test.Unit.QuickCheck.quickCheck \value options ->
        let
          result :: Either (Array String) (Maybe String)
          result =
            Form2.validate
              (Form2.Ocelot.Dropdown.dropdown { display: identity, options, placeholder: "" } :: Form2.Form _ _ Data.Identity.Identity _ _)
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
    Test.Unit.test "`dropdown` should not change its value if `readonly = true`" do
      Test.Unit.QuickCheck.quickCheck \options readonly -> do
        value <- Control.Monad.Gen.Common.genMaybe (Test.QuickCheck.Gen.elements options)
        let
          rendered :: Form2.Ocelot.Dropdown.Render (Maybe String -> Maybe String)
          rendered =
            Form2.Render.match { dropdown: map (un Data.Identity.Identity) }
              $ Form2.render
                  ( Form2.Ocelot.Dropdown.dropdown
                      { display: identity
                      , options: Data.Array.NonEmpty.toArray options
                      , placeholder: ""
                      }
                  )
                  { readonly }
                  value
        Form2.Ocelot.Dropdown.withRender rendered \render -> do
          value' <-
            Test.QuickCheck.Gen.elements
              -- We know that `render.options` is a non-empty array because we
              -- have generated it as such, so it's safe to use
              -- `Partia.Unsafe.unsafePartial` here.
              $ Partial.Unsafe.unsafePartial
              $ Data.Maybe.fromJust
              $ Data.Array.NonEmpty.fromArray
              $ render.options
          let
            expected :: Maybe String
            expected = if readonly then value else map (render.display <<< Just) value'
          pure $ expected === render.onChange value' value
    Test.Unit.test "`dropdown` should be cleared when selecting the first (empty) option" do
      Test.Unit.QuickCheck.quickCheck \value options ->
        let
          rendered :: Form2.Ocelot.Dropdown.Render (Maybe String -> Maybe String)
          rendered =
            Form2.Render.match { dropdown: map (un Data.Identity.Identity) }
              $ Form2.render
                  ( Form2.Ocelot.Dropdown.dropdown
                      { display: identity
                      , options
                      , placeholder: ""
                      }
                  )
                  { readonly: false }
                  value

          expected :: Maybe String
          expected = Nothing
        in
          Form2.Ocelot.Dropdown.withRender rendered \render ->
            expected === render.onChange Nothing value

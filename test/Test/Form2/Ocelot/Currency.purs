module Test.Formlet.Ocelot.Currency
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Identity as Data.Identity
import Formlet as Formlet
import Formlet.Ocelot.Currency as Formlet.Ocelot.Currency
import Formlet.Ocelot.Text as Formlet.Ocelot.Text
import Formlet.Render as Formlet.Render
import Ocelot.Data.Currency as Ocelot.Data.Currency
import Test.QuickCheck ((===))
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Formlet.Ocelot.Currency" do
    Test.Unit.test "`currency` sets the `addonLeft` of the rendered `Formlet.Ocelot.Text.Render` if the `symbol` param is passed, or to \"$\" as the default" do
      Test.Unit.QuickCheck.quickCheck \value symbol ->
        let
          rendered :: Formlet.Ocelot.Text.Render (String -> String)
          rendered =
            Formlet.Render.match { text: map (un Data.Identity.Identity) }
              $ Formlet.render (Formlet.Ocelot.Currency.currency { symbol }) { readonly: false }
              $ value

          expected :: Maybe String
          expected = Just symbol

          actual :: Maybe String
          actual = (un Formlet.Ocelot.Text.Render rendered).addonLeft
        in
          expected === actual
    Test.Unit.test "`currency` should be valid if the value can be parsed into Cents from a Dollar string" do
      Test.Unit.QuickCheck.quickCheck \int ->
        let
          cents :: Ocelot.Data.Currency.Cents
          cents = Ocelot.Data.Currency.centsFromInt int

          expected :: Either (Array String) (Maybe Ocelot.Data.Currency.Cents)
          expected = Right (Just cents)

          actual :: Either (Array String) (Maybe Ocelot.Data.Currency.Cents)
          actual =
            Formlet.validate testForm { readonly: false }
              $ Ocelot.Data.Currency.formatCentsToStrDollars
              $ cents
        in
          expected === actual

testForm ::
  forall config options renders.
  Formlet.Form
    { readonly :: Boolean | config }
    (Formlet.Render.Render (errors :: Formlet.Errors, required :: Boolean | options) (text :: Formlet.Ocelot.Text.Render | renders))
    Data.Identity.Identity
    String
    (Maybe Ocelot.Data.Currency.Cents)
testForm = Formlet.Ocelot.Currency.currency { symbol: "$" }

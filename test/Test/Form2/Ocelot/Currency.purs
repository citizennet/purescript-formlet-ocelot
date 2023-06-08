module Test.Form2.Ocelot.Currency
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Identity as Data.Identity
import Form2 as Form2
import Form2.Ocelot.Currency as Form2.Ocelot.Currency
import Form2.Ocelot.Text as Form2.Ocelot.Text
import Form2.Render as Form2.Render
import Ocelot.Data.Currency as Ocelot.Data.Currency
import Test.QuickCheck ((===))
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Form2.Ocelot.Currency" do
    Test.Unit.test "`currency` sets the `addonLeft` of the rendered `Form2.Ocelot.Text.Render` if the `symbol` param is passed, or to \"$\" as the default" do
      Test.Unit.QuickCheck.quickCheck \value symbol ->
        let
          rendered :: Form2.Ocelot.Text.Render (String -> String)
          rendered =
            Form2.Render.match { text: map (un Data.Identity.Identity) }
              $ Form2.render (Form2.Ocelot.Currency.currency { symbol }) { readonly: false }
              $ value

          expected :: Maybe String
          expected = Just symbol

          actual :: Maybe String
          actual = (un Form2.Ocelot.Text.Render rendered).addonLeft
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
            Form2.validate testForm { readonly: false }
              $ Ocelot.Data.Currency.formatCentsToStrDollars
              $ cents
        in
          expected === actual

testForm ::
  forall config options renders.
  Form2.Form
    { readonly :: Boolean | config }
    (Form2.Render.Render (errors :: Form2.Errors, required :: Boolean | options) (text :: Form2.Ocelot.Text.Render | renders))
    Data.Identity.Identity
    String
    (Maybe Ocelot.Data.Currency.Cents)
testForm = Form2.Ocelot.Currency.currency { symbol: "$" }

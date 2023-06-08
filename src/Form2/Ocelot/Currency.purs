module Form2.Ocelot.Currency
  ( Params
  , ParamsOptional
  , ParamsRequired
  , currency
  ) where

import CitizenNet.Prelude

import Data.String.NonEmpty as Data.String.NonEmpty
import Form2 as Form2
import Form2.Ocelot.Text as Form2.Ocelot.Text
import Form2.Render as Form2.Render
import Form2.Validation as Form2.Validation
import Ocelot.Data.Currency as Ocelot.Data.Currency
import Option as Option

type Params =
  ( placeholder :: Maybe String
  , symbol :: String
  )

type ParamsOptional =
  ( placeholder :: String
  )

type ParamsRequired =
  ( symbol :: String
  )

currency ::
  forall config options params renders m.
  Applicative m =>
  Option.FromRecord params ParamsRequired ParamsOptional =>
  Record params ->
  Form2.Form
    { readonly :: Boolean | config }
    ( Form2.Render.Render
        (errors :: Form2.Errors, required :: Boolean | options)
        (text :: Form2.Ocelot.Text.Render | renders)
    )
    m
    String
    (Maybe Ocelot.Data.Currency.Cents)
currency params' =
  Form2.Validation.validated (Form2.Validation.optional Data.String.NonEmpty.fromString centsValidator)
    $ Form2.Ocelot.Text.text
        { addonLeft: params.symbol
        , placeholder: params.placeholder
        }
  where
  centsValidator :: Form2.Validation.Validator Data.String.NonEmpty.NonEmptyString Ocelot.Data.Currency.Cents
  centsValidator =
    Form2.Validation.NotRequired
      $ note "Invalid currency value"
      <<< Ocelot.Data.Currency.parseCentsFromDollarStr
      <<< Data.String.NonEmpty.toString

  params :: Record Params
  params =
    Option.recordToRecord
      ( Option.recordFromRecord params' ::
          Option.Record ParamsRequired ParamsOptional
      )

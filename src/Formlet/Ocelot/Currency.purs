module Formlet.Ocelot.Currency
  ( Params
  , ParamsOptional
  , ParamsRequired
  , currency
  ) where

import CitizenNet.Prelude

import Data.String.NonEmpty as Data.String.NonEmpty
import Formlet as Formlet
import Formlet.Ocelot.Text as Formlet.Ocelot.Text
import Formlet.Render as Formlet.Render
import Formlet.Validation as Formlet.Validation
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
  Formlet.Form
    { readonly :: Boolean | config }
    ( Formlet.Render.Render
        (errors :: Formlet.Errors, required :: Boolean | options)
        (text :: Formlet.Ocelot.Text.Render | renders)
    )
    m
    String
    (Maybe Ocelot.Data.Currency.Cents)
currency params' =
  Formlet.Validation.validated (Formlet.Validation.optional Data.String.NonEmpty.fromString centsValidator)
    $ Formlet.Ocelot.Text.text
        { addonLeft: params.symbol
        , placeholder: params.placeholder
        }
  where
  centsValidator :: Formlet.Validation.Validator Data.String.NonEmpty.NonEmptyString Ocelot.Data.Currency.Cents
  centsValidator =
    Formlet.Validation.NotRequired
      $ note "Invalid currency value"
      <<< Ocelot.Data.Currency.parseCentsFromDollarStr
      <<< Data.String.NonEmpty.toString

  params :: Record Params
  params =
    Option.recordToRecord
      ( Option.recordFromRecord params' ::
          Option.Record ParamsRequired ParamsOptional
      )

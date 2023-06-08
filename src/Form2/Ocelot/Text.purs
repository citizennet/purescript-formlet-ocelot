module Formlet.Ocelot.Text
  ( Render(..)
  , text
  ) where

import CitizenNet.Prelude

import Formlet as Formlet
import Formlet.Render as Formlet.Render
import Option as Option

type Params =
  ( addonLeft :: Maybe String
  , placeholder :: Maybe String
  )

type ParamsOptional =
  ( addonLeft :: String
  , placeholder :: String
  )

type ParamsRequired =
  () :: Row Type

newtype Render action =
  Render
    { addonLeft :: Maybe String
    , onChange :: String -> action
    , placeholder :: String
    , readonly :: Boolean
    , value :: String
    }

derive instance newtypeText :: Newtype (Render action) _
derive instance functorText :: Functor Render

text ::
  forall config options polyParams renders m.
  Applicative m =>
  Option.FromRecord polyParams ParamsRequired ParamsOptional =>
  Option.ToRecord ParamsRequired ParamsOptional Params =>
  Record polyParams ->
  Formlet.Form { readonly :: Boolean | config } (Formlet.Render.Render options (text :: Render | renders)) m String String
text polyParams =
  Formlet.form_ \({ readonly }) value ->
    Formlet.Render.inj
      { text:
          Render
            { addonLeft: params.addonLeft
            , onChange: if readonly then const (pure identity) else pure <<< const
            , placeholder: fromMaybe "" params.placeholder
            , readonly
            , value
            }
      }
  where
  params :: Record Params
  params =
    Option.recordToRecord
      ( optionRecord polyParams ::
          OptionRecord ParamsRequired ParamsOptional
      )

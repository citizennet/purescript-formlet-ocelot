module Formlet.Ocelot.Textarea
  ( Render(..)
  , textarea
  ) where

import CitizenNet.Prelude

import Formlet as Formlet
import Formlet.Render as Formlet.Render
import Option as Option

type Params =
  ( placeholder :: Maybe String
  , rows :: Int
  )

type ParamsOptional =
  ( placeholder :: String
  )

type ParamsRequired =
  ( rows :: Int
  )

newtype Render action =
  Render
    { onChange :: String -> action
    , placeholder :: String
    , readonly :: Boolean
    , rows :: Int
    , value :: String
    }

derive instance newtypeTextarea :: Newtype (Render action) _
derive instance functorTextarea :: Functor Render

textarea ::
  forall config options polyParams renders m.
  Applicative m =>
  Option.FromRecord polyParams ParamsRequired ParamsOptional =>
  Option.ToRecord ParamsRequired ParamsOptional Params =>
  Record polyParams ->
  Formlet.Form { readonly :: Boolean | config } (Formlet.Render.Render options (textarea :: Render | renders)) m String String
textarea polyParams =
  Formlet.form_ \({ readonly }) value ->
    Formlet.Render.inj
      { textarea:
          Render
            { onChange: if readonly then const (pure identity) else pure <<< const
            , placeholder: fromMaybe "" params.placeholder
            , readonly
            , rows: params.rows
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

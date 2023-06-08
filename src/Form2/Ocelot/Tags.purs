module Formlet.Ocelot.Tags
  ( Render(..)
  , tags
  ) where

import CitizenNet.Prelude

import Formlet as Formlet
import Formlet.Render as Formlet.Render
import Option as Option

type Params =
  ( items :: Maybe (Array String)
  , minWidth :: Maybe Number
  , placeholder ::
      { primary :: String
      , secondary :: String
      }
  )

type ParamsOptional =
  ( items :: Array String
  , minWidth :: Number
  )

type ParamsRequired =
  ( placeholder ::
      { primary :: String
      , secondary :: String
      }
  )

newtype Render action = Render
  { minWidth :: Number {- px -}
  , onChange :: Array String -> action
  , items :: Array String
  , placeholder ::
      { primary :: String
      , secondary :: String
      }
  , readonly :: Boolean
  , value :: Array String
  }

derive instance newtypeText :: Newtype (Render action) _

derive instance functorText :: Functor Render

tags ::
  forall config options polyParams renders m.
  Applicative m =>
  Option.FromRecord polyParams ParamsRequired ParamsOptional =>
  Option.ToRecord ParamsRequired ParamsOptional Params =>
  Record polyParams ->
  Formlet.Form
    { readonly :: Boolean | config }
    (Formlet.Render.Render options (tags :: Render | renders))
    m
    (Array String)
    (Array String)
tags polyParams =
  Formlet.form_ \config value ->
    Formlet.Render.inj
      { tags:
          Render
            { items: fromMaybe [] params.items
            , minWidth: fromMaybe 50.0 params.minWidth
            , onChange:
                if config.readonly then
                  const (pure identity)
                else
                  pure <<< const
            , placeholder: params.placeholder
            , readonly: config.readonly
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

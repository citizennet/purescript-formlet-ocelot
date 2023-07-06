module Formlet.Ocelot.Toggle
  ( Render(..)
  , toggle
  ) where

import CitizenNet.Prelude

import Formlet as Formlet
import Formlet.Render as Formlet.Render
import Option as Option

type Params =
  ( label :: Maybe String
  )

type ParamsOptional =
  ( label :: String
  )

type ParamsRequired :: forall k. Row k
type ParamsRequired = ()

newtype Render action =
  Render
    { label :: Maybe String
    , onChange :: Boolean -> action
    , readonly :: Boolean
    , value :: Boolean
    }

derive instance newtypeToggle :: Newtype (Render action) _
derive instance functorToggle :: Functor Render

toggle ::
  forall config options polyParams renders m.
  Applicative m =>
  Option.FromRecord polyParams ParamsRequired ParamsOptional =>
  Option.ToRecord ParamsRequired ParamsOptional Params =>
  Record polyParams ->
  Formlet.Form { readonly :: Boolean | config } (Formlet.Render.Render options (toggle :: Render | renders)) m Boolean Boolean
toggle polyParams =
  Formlet.form_ \{ readonly } value ->
    Formlet.Render.inj
      { toggle:
          Render
            { label: params.label
            , onChange: if readonly then const (pure identity) else pure <<< const
            , readonly
            , value
            }
      }
  where
  params :: Record Params
  params =
    Option.recordToRecord (optionRecord polyParams :: OptionRecord ParamsRequired ParamsOptional)

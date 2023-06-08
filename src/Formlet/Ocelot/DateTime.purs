module Formlet.Ocelot.DateTime
  ( Render(..)
  , dateTime
  ) where

import CitizenNet.Prelude

import Formlet as Formlet
import Formlet.Render as Formlet.Render
import TimeZone as TimeZone

newtype Render action =
  Render
    { onChange :: Maybe DateTime -> action
    , placeholder :: String
    , readonly :: Boolean
    , timezone :: TimeZone.TimeZone
    , value :: Maybe DateTime
    }

derive instance newtypeDateTime :: Newtype (Render action) _
derive instance functorDateTime :: Functor Render

dateTime ::
  forall config options renders m.
  Applicative m =>
  { placeholder :: String } ->
  Formlet.Form
    { readonly :: Boolean
    , timezone :: TimeZone.TimeZone
    | config
    }
    (Formlet.Render.Render options (dateTime :: Render | renders))
    m
    (Maybe DateTime)
    (Maybe DateTime)
dateTime { placeholder } =
  Formlet.form_ \{ readonly, timezone } value ->
    Formlet.Render.inj
      { dateTime:
          Render
            { onChange: if readonly then const (pure identity) else pure <<< const
            , placeholder
            , readonly
            , timezone
            , value
            }
      }

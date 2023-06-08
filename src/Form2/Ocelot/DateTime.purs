module Form2.Ocelot.DateTime
  ( Render(..)
  , dateTime
  ) where

import CitizenNet.Prelude

import Form2 as Form2
import Form2.Render as Form2.Render
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
  Form2.Form
    { readonly :: Boolean
    , timezone :: TimeZone.TimeZone
    | config
    }
    (Form2.Render.Render options (dateTime :: Render | renders))
    m
    (Maybe DateTime)
    (Maybe DateTime)
dateTime { placeholder } =
  Form2.form_ \{ readonly, timezone } value ->
    Form2.Render.inj
      { dateTime:
          Render
            { onChange: if readonly then const (pure identity) else pure <<< const
            , placeholder
            , readonly
            , timezone
            , value
            }
      }

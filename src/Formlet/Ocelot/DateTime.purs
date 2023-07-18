module Formlet.Ocelot.DateTime
  ( Interval
  , Render(..)
  , dateTime
  ) where

import CitizenNet.Prelude

import Formlet as Formlet
import Formlet.Render as Formlet.Render
import Option as Option
import TimeZone as TimeZone

type Interval =
  { end :: Maybe DateTime
  , start :: Maybe DateTime
  }

type Params =
  ( interval :: Maybe Interval
  , placeholder :: Maybe String
  )

type ParamsOptional =
  ( interval :: Interval
  , placeholder :: String
  )

type ParamsRequired =
  () :: Row Type

newtype Render action =
  Render
    { interval :: Maybe Interval
    , onChange :: Maybe DateTime -> action
    , placeholder :: String
    , readonly :: Boolean
    , timezone :: TimeZone.TimeZone
    , value :: Maybe DateTime
    }

derive instance newtypeDateTime :: Newtype (Render action) _
derive instance functorDateTime :: Functor Render

dateTime ::
  forall config options polyParams renders m.
  Applicative m =>
  Option.FromRecord polyParams ParamsRequired ParamsOptional =>
  Option.ToRecord ParamsRequired ParamsOptional Params =>
  Record polyParams ->
  Formlet.Form
    { readonly :: Boolean
    , timezone :: TimeZone.TimeZone
    | config
    }
    (Formlet.Render.Render options (dateTime :: Render | renders))
    m
    (Maybe DateTime)
    (Maybe DateTime)
dateTime polyParams =
  Formlet.form_ \({ readonly, timezone }) value ->
    Formlet.Render.inj
      { dateTime:
          Render
            { interval: params.interval
            , onChange: if readonly then const (pure identity) else pure <<< const
            , placeholder: fromMaybe "" params.placeholder
            , readonly
            , timezone
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

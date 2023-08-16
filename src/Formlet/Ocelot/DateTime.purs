module Formlet.Ocelot.DateTime
  ( Interval
  , Render(..)
  , dateTime
  ) where

import CitizenNet.Prelude

import Data.Time as Data.Time
import Formlet as Formlet
import Formlet.Render as Formlet.Render
import Option as Option
import TimeZone as TimeZone

type Interval =
  { end :: Maybe DateTime
  , start :: Maybe DateTime
  }

type Params =
  ( defaultTime :: Maybe Data.Time.Time
  , interval :: Maybe Interval
  )

type ParamsOptional =
  ( defaultTime :: Data.Time.Time
  , interval :: Interval
  )

type ParamsRequired =
  () :: Row Type

newtype Render action =
  Render
    { defaultTime :: Maybe Data.Time.Time
    , interval :: Maybe Interval
    , onChange :: Maybe DateTime -> action
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
            { defaultTime: params.defaultTime
            , interval: params.interval
            , onChange: if readonly then const (pure identity) else pure <<< const
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

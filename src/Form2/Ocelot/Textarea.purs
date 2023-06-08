module Form2.Ocelot.Textarea
  ( Render(..)
  , textarea
  ) where

import CitizenNet.Prelude

import Form2 as Form2
import Form2.Render as Form2.Render
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
  Form2.Form { readonly :: Boolean | config } (Form2.Render.Render options (textarea :: Render | renders)) m String String
textarea polyParams =
  Form2.form_ \({ readonly }) value ->
    Form2.Render.inj
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

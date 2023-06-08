module Form2.Ocelot.Text
  ( Render(..)
  , text
  ) where

import CitizenNet.Prelude

import Form2 as Form2
import Form2.Render as Form2.Render
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
  Form2.Form { readonly :: Boolean | config } (Form2.Render.Render options (text :: Render | renders)) m String String
text polyParams =
  Form2.form_ \({ readonly }) value ->
    Form2.Render.inj
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

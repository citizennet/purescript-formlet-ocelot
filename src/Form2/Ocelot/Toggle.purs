module Form2.Ocelot.Toggle
  ( Render(..)
  , toggle
  ) where

import CitizenNet.Prelude

import Form2 as Form2
import Form2.Render as Form2.Render

newtype Render action =
  Render
    { onChange :: Boolean -> action
    , readonly :: Boolean
    , value :: Boolean
    }

derive instance newtypeToggle :: Newtype (Render action) _
derive instance functorToggle :: Functor Render

toggle ::
  forall config options renders m.
  Applicative m =>
  Form2.Form { readonly :: Boolean | config } (Form2.Render.Render options (toggle :: Render | renders)) m Boolean Boolean
toggle =
  Form2.form_ \{ readonly } value ->
    Form2.Render.inj
      { toggle:
          Render
            { onChange: if readonly then const (pure identity) else pure <<< const
            , readonly
            , value
            }
      }

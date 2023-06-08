module Formlet.Ocelot.Toggle
  ( Render(..)
  , toggle
  ) where

import CitizenNet.Prelude

import Formlet as Formlet
import Formlet.Render as Formlet.Render

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
  Formlet.Form { readonly :: Boolean | config } (Formlet.Render.Render options (toggle :: Render | renders)) m Boolean Boolean
toggle =
  Formlet.form_ \{ readonly } value ->
    Formlet.Render.inj
      { toggle:
          Render
            { onChange: if readonly then const (pure identity) else pure <<< const
            , readonly
            , value
            }
      }

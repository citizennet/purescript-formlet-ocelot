module Formlet.Ocelot.Toggle.Halogen
  ( render
  ) where

import CitizenNet.Prelude

import Formlet.Ocelot.Toggle as Formlet.Ocelot.Toggle
import Halogen as Halogen
import Halogen.HTML.Events as Halogen.HTML.Events
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.Block.Toggle as Ocelot.Block.Toggle

render ::
  forall slots m config action.
  { key :: String } ->
  { readonly :: Boolean | config } ->
  Formlet.Ocelot.Toggle.Render action ->
  Array (Halogen.ComponentHTML action slots m)
render { key } { readonly } (Formlet.Ocelot.Toggle.Render render') =
  [ Ocelot.Block.Toggle.toggle
      [ Halogen.HTML.Properties.checked render'.value
      , Halogen.HTML.Properties.disabled (readonly || render'.readonly)
      , Halogen.HTML.Events.onChecked render'.onChange
      , Halogen.HTML.Properties.id key
      , Halogen.HTML.Properties.name key
      ]
  ]

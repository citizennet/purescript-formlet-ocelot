module Formlet.Ocelot.Checkbox.Halogen
  ( render
  ) where

import CitizenNet.Prelude

import Formlet.Ocelot.Checkbox as Formlet.Ocelot.Checkbox
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Events as Halogen.HTML.Events
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.Block.Checkbox as Ocelot.Block.Checkbox

render ::
  forall slots m config action.
  { key :: String } ->
  { readonly :: Boolean | config } ->
  Formlet.Ocelot.Checkbox.Render action ->
  Array (Halogen.ComponentHTML action slots m)
render { key } { readonly } (Formlet.Ocelot.Checkbox.Render { options }) =
  options
    # map \option ->
        Ocelot.Block.Checkbox.checkbox_
          [ Halogen.HTML.Events.onChecked option.onChange
          , Halogen.HTML.Properties.checked option.checked
          , Halogen.HTML.Properties.disabled (readonly || option.readonly)
          , Halogen.HTML.Properties.name key
          ]
          [ Halogen.HTML.text option.label
          ]

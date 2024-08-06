module Formlet.Ocelot.Radio.Halogen
  ( render
  ) where

import CitizenNet.Prelude

import Formlet.Ocelot.Radio as Formlet.Ocelot.Radio
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Events as Halogen.HTML.Events
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.Block.Radio as Ocelot.Block.Radio

render ::
  forall slots m config action.
  { readonly :: Boolean | config } ->
  Formlet.Ocelot.Radio.Render action ->
  Array (Halogen.ComponentHTML action slots m)
render { readonly } (Formlet.Ocelot.Radio.Render render') =
  render'.options
    # map \({ label, onSelect }) ->
        Ocelot.Block.Radio.radio_
          [ Halogen.HTML.Events.onClick \_ -> onSelect
          , Halogen.HTML.Properties.checked (render'.value == Just label)
          , Halogen.HTML.Properties.disabled (readonly || render'.readonly)
          ]
          [ Halogen.HTML.text label ]

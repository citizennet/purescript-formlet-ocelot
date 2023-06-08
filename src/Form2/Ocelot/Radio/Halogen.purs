module Form2.Ocelot.Radio.Halogen
  ( render
  ) where

import CitizenNet.Prelude

import Form2.Ocelot.Radio as Form2.Ocelot.Radio
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Events as Halogen.HTML.Events
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.Block.Radio as Ocelot.Block.Radio

render ::
  forall slots m config action.
  { key :: String } ->
  { readonly :: Boolean | config } ->
  Form2.Ocelot.Radio.Render action ->
  Array (Halogen.ComponentHTML action slots m)
render { key } { readonly } (Form2.Ocelot.Radio.Render render') =
  render'.options
    # map \{ label, onSelect } ->
        Ocelot.Block.Radio.radio_
          [ Halogen.HTML.Events.onClick \_ -> onSelect
          , Halogen.HTML.Properties.checked (render'.value == Just label)
          , Halogen.HTML.Properties.disabled (readonly || render'.readonly)
          , Halogen.HTML.Properties.name key
          ]
          [ Halogen.HTML.text label ]

module Formlet.Ocelot.Textarea.Halogen
  ( render
  ) where

import CitizenNet.Prelude

import Formlet.Ocelot.Textarea as Formlet.Ocelot.Textarea
import Halogen as Halogen
import Halogen.HTML.Events as Halogen.HTML.Events
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.Block.Input as Ocelot.Block.Input

render ::
  forall slots m config action.
  { key :: String } ->
  { readonly :: Boolean | config } ->
  Formlet.Ocelot.Textarea.Render action ->
  Array (Halogen.ComponentHTML action slots m)
render { key } { readonly } (Formlet.Ocelot.Textarea.Render render') =
  [ Ocelot.Block.Input.textarea
      [ Halogen.HTML.Properties.value render'.value
      , Halogen.HTML.Properties.placeholder render'.placeholder
      , Halogen.HTML.Properties.rows render'.rows
      , Halogen.HTML.Properties.disabled (readonly || render'.readonly)
      , Halogen.HTML.Events.onValueChange render'.onChange
      , Halogen.HTML.Properties.id key
      , Halogen.HTML.Properties.name key
      ]
  ]

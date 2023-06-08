module Formlet.Ocelot.Text.Halogen
  ( render
  ) where

import CitizenNet.Prelude

import Formlet.Ocelot.Text as Formlet.Ocelot.Text
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Events as Halogen.HTML.Events
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.Block.Input as Ocelot.Block.Input

render ::
  forall slots m config action.
  { key :: String } ->
  { readonly :: Boolean | config } ->
  Formlet.Ocelot.Text.Render action ->
  Array (Halogen.ComponentHTML action slots m)
render { key } { readonly } (Formlet.Ocelot.Text.Render render') = case render'.addonLeft of
  Nothing ->
    [ Ocelot.Block.Input.input inputProps
    ]
  Just addonLeft ->
    [ Ocelot.Block.Input.inputGroup_
        [ Ocelot.Block.Input.inputRight inputProps
        , Ocelot.Block.Input.addonLeft_
            [ Halogen.HTML.text addonLeft ]
        ]
    ]
  where
  inputProps =
    [ Halogen.HTML.Properties.value render'.value
    , Halogen.HTML.Properties.placeholder render'.placeholder
    , Halogen.HTML.Properties.disabled (readonly || render'.readonly)
    , Halogen.HTML.Events.onValueChange render'.onChange
    , Halogen.HTML.Events.onBlur \_ -> render'.onChange render'.value
    , Halogen.HTML.Properties.id key
    , Halogen.HTML.Properties.name key
    ]

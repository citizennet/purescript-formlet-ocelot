module Formlet.Ocelot.Checkbox.Halogen
  ( render
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Foreign.Object as Foreign.Object
import Formlet.Ocelot.Checkbox as Formlet.Ocelot.Checkbox
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Events as Halogen.HTML.Events
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.Block.Checkbox as Ocelot.Block.Checkbox
import Ocelot.HTML.Properties as Ocelot.HTML.Properties

render ::
  forall slots m config action.
  { key :: String } ->
  { readonly :: Boolean | config } ->
  Formlet.Ocelot.Checkbox.Render action ->
  Array (Halogen.ComponentHTML action slots m)
render { key } { readonly } (Formlet.Ocelot.Checkbox.Render { columns, options }) =
  case Data.Array.uncons columns of
    Nothing -> checkboxes
    Just { head: numColumns } ->
      [ Halogen.HTML.div
          [ Ocelot.HTML.Properties.style
              $ Foreign.Object.fromHomogeneous
                  { "display": "grid"
                  , "gap": "0.75rem"
                  , "grid-template-columns": "repeat(" <> show numColumns <> ", 1fr)"
                  }
          ]
          checkboxes
      ]
  where
  checkboxes :: Array (Halogen.ComponentHTML action slots m)
  checkboxes =
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

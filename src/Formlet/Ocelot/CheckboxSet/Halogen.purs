module Formlet.Ocelot.CheckboxSet.Halogen
  ( render
  ) where

import CitizenNet.Prelude

import Foreign.Object as Foreign.Object
import Formlet.Ocelot.Checkbox as Formlet.Ocelot.Checkbox
import Formlet.Ocelot.Checkbox.Halogen as Formlet.Ocelot.Checkbox.Halogen
import Formlet.Ocelot.CheckboxSet as Formlet.Ocelot.CheckboxSet
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Ocelot.HTML.Properties as Ocelot.HTML.Properties

render ::
  forall slots m config action.
  { key :: String } ->
  { readonly :: Boolean | config } ->
  Formlet.Ocelot.CheckboxSet.Render action ->
  Array (Halogen.ComponentHTML action slots m)
render { key } { readonly } (Formlet.Ocelot.CheckboxSet.Render { columns, options }) =
  if columns <= 1 then
    checkboxes
  else
    [ Halogen.HTML.div
        [ Ocelot.HTML.Properties.style
            $ Foreign.Object.fromHomogeneous
                { "display": "grid"
                , "gap": "0.75rem"
                , "grid-template-columns": "repeat(" <> show columns <> ", 1fr)"
                }
        ]
        checkboxes
    ]
  where
  checkboxes :: Array (Halogen.ComponentHTML action slots m)
  checkboxes =
    options
      # foldMap \option ->
          Formlet.Ocelot.Checkbox.Halogen.render
            { key }
            { readonly }
            (Formlet.Ocelot.Checkbox.Render [ option ])

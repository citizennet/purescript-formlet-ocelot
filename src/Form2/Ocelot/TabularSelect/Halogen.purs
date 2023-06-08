module Form2.Ocelot.TabularSelect.Halogen
  ( render
  ) where

import CitizenNet.Prelude

import Data.Monoid as Data.Monoid
import Foreign.Object as Foreign.Object
import Form2.Ocelot.TabularSelect as Form2.Ocelot.TabularSelect
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Events as Halogen.HTML.Events
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.Block.Hover as Ocelot.Block.Hover
import Ocelot.Block.Icon as Ocelot.Block.Icon
import Ocelot.HTML.Properties as Ocelot.HTML.Properties

render ::
  forall slots m config action.
  { key :: String } ->
  { readonly :: Boolean | config } ->
  Form2.Ocelot.TabularSelect.Render action ->
  Array (Halogen.ComponentHTML action slots m)
render { key } { readonly } (Form2.Ocelot.TabularSelect.Render render') =

  [ Halogen.HTML.div
      [ Ocelot.HTML.Properties.style
          $ Foreign.Object.fromHomogeneous
              { "display": "grid"
              , "gap": "0.75rem"
              , "grid-template-columns": "repeat(" <> show render'.columns <> ", 1fr)"
              }
      ]
      ( render'.options <#> \({ error: maybeError, label, onSelect }) ->
          Halogen.HTML.button
            [ Halogen.HTML.Properties.classes (itemClasses (render'.value == Just label))
            , Halogen.HTML.Events.onClick \_ -> onSelect
            , Halogen.HTML.Properties.disabled (readonly || render'.readonly)
            , Halogen.HTML.Properties.name key
            ]
            [ Halogen.HTML.text label
            , case maybeError of
                Nothing -> Halogen.HTML.text ""
                Just error ->
                  Ocelot.Block.Hover.hover_
                    [ Ocelot.Block.Hover.Bottom ]
                    ( Halogen.HTML.span
                        [ Ocelot.HTML.Properties.css "ml-2 text-red" ]
                        [ Ocelot.Block.Icon.error_ ]
                    )
                    ( ( Halogen.HTML.div
                          [ Ocelot.HTML.Properties.css "bg-black-10 opacity-75 px-2 rounded shadow-md text-white"
                          ]
                          [ Halogen.HTML.p
                              [ Ocelot.HTML.Properties.css "whitespace-no-wrap" ]
                              [ Halogen.HTML.text error ]
                          ]
                      )
                    )
            ]
      )
  ]
  where
  itemClasses :: Boolean -> Array Halogen.ClassName
  itemClasses selected =
    [ "border-2"
    , if selected then "border-blue-88" else "border-grey-88"
    , "disabled:cursor-default"
    , "!disabled:cursor-pointer"
    , "disabled:opacity-50"
    , Data.Monoid.guard selected "font-medium"
    , "hover:shadow"
    , "no-outline"
    , "px-4"
    , "py-2"
    , "rounded"
    , "text-black-20"
    ]
      <#> Halogen.ClassName

module Form2.Ocelot.Array.Halogen
  ( render
  ) where

import CitizenNet.Prelude

import Form2.Ocelot.Array as Form2.Ocelot.Array
import Form2.Render.List as Form2.Render.List
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Elements.Keyed as Halogen.HTML.Elements.Keyed
import Halogen.HTML.Events as Halogen.HTML.Events
import Ocelot.Block.Icon as Ocelot.Block.Icon
import Ocelot.Button as Ocelot.Button
import Ocelot.HTML.Properties as Ocelot.HTML.Properties

render ::
  forall action config m render slots.
  Applicative m =>
  (Form2.Render.List.Key -> config -> render action -> Halogen.ComponentHTML action slots m) ->
  config ->
  Form2.Ocelot.Array.Render render action ->
  Array (Halogen.ComponentHTML action slots m)
render renderElement config (Form2.Ocelot.Array.Render render') =
  [ Halogen.HTML.Elements.Keyed.div
      [ Ocelot.HTML.Properties.css "w-full" ]
      $ map (\{ key, render: row } -> Tuple key (renderRow renderElement config key row))
      $ un Form2.Render.List.List render'.rows
  , case render'.onAddSection of
      Nothing -> Halogen.HTML.text ""
      Just onAddSection ->
        Ocelot.Button.button
          [ Halogen.HTML.Events.onClick \_ -> onAddSection
          ]
          [ Ocelot.Block.Icon.plus_
          , Halogen.HTML.span
              [ Ocelot.HTML.Properties.css "pl-2"
              ]
              [ Halogen.HTML.text "Add"
              ]
          ]
  ]

renderRow ::
  forall action config m render slots.
  Applicative m =>
  (Form2.Render.List.Key -> config -> render action -> Halogen.ComponentHTML action slots m) ->
  config ->
  Form2.Render.List.Key ->
  Form2.Ocelot.Array.RenderRow render action ->
  Halogen.ComponentHTML action slots m
renderRow renderElement config key (Form2.Ocelot.Array.RenderRow row) =
  Halogen.HTML.div
    [ Ocelot.HTML.Properties.css "flex items-center my-1" ]
    [ renderElement key config row.render
    , case row.onRemove of
        Nothing -> Halogen.HTML.text ""
        Just onRemove ->
          Ocelot.Button.buttonClear
            [ Ocelot.HTML.Properties.css "my-1"
            , Halogen.HTML.Events.onClick \_ -> onRemove
            ]
            [ Ocelot.Block.Icon.close_ ]
    ]

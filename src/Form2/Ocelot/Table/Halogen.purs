module Form2.Ocelot.Table.Halogen
  ( render
  ) where

import CitizenNet.Prelude

import Form2.Ocelot.Table as Form2.Ocelot.Table
import Form2.Render.List as Form2.Render.List
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Ocelot.Block.Table as Ocelot.Block.Table

render ::
  forall action m render slots.
  MonadAff m =>
  ( { column :: Form2.Render.List.Key
    , row :: Form2.Render.List.Key
    } ->
    render action ->
    Halogen.ComponentHTML action slots m
  ) ->
  Form2.Ocelot.Table.Render render action ->
  Array (Halogen.ComponentHTML action slots m)
render renderElement = case _ of
  Form2.Ocelot.Table.Render { header, rows } ->
    [ renderHeader header ]
      <> renderRows renderElement rows

renderHeader ::
  forall action m slots.
  Array String ->
  Halogen.ComponentHTML action slots m
renderHeader =
  Ocelot.Block.Table.row_
    <<< map renderHeaderCell

renderHeaderCell ::
  forall action m slots.
  String ->
  Halogen.ComponentHTML action slots m
renderHeaderCell title =
  Ocelot.Block.Table.header_
    [ Halogen.HTML.text title ]

renderRows ::
  forall action m render slots.
  ( { column :: Form2.Render.List.Key
    , row :: Form2.Render.List.Key
    } ->
    render action ->
    Halogen.ComponentHTML action slots m
  ) ->
  Form2.Render.List.List (Form2.Ocelot.Table.RenderRow render) action ->
  Array (Halogen.ComponentHTML action slots m)
renderRows renderElement = case _ of
  Form2.Render.List.List keyedRenderRows -> do
    keyedRenderRows
      <#> \({ key, render: row }) ->
        renderRow (\{ column } -> renderElement { column, row: key }) row

renderRow ::
  forall action m render slots.
  ( { column :: Form2.Render.List.Key } ->
    render action ->
    Halogen.ComponentHTML action slots m
  ) ->
  Form2.Ocelot.Table.RenderRow render action ->
  Halogen.ComponentHTML action slots m
renderRow renderElement = case _ of
  Form2.Ocelot.Table.RenderRow { columns } -> do
    Ocelot.Block.Table.row_
      (renderColumns renderElement columns)

renderColumns ::
  forall action m render slots.
  ( { column :: Form2.Render.List.Key } ->
    render action ->
    Halogen.ComponentHTML action slots m
  ) ->
  Form2.Render.List.List render action ->
  Array (Halogen.ComponentHTML action slots m)
renderColumns renderElement = case _ of
  Form2.Render.List.List keyedRenders ->
    keyedRenders
      <#> \({ key, render: column }) ->
        Ocelot.Block.Table.cell_
          [ renderElement { column: key } column ]

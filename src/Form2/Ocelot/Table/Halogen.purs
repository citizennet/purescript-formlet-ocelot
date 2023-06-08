module Formlet.Ocelot.Table.Halogen
  ( render
  ) where

import CitizenNet.Prelude

import Formlet.Ocelot.Table as Formlet.Ocelot.Table
import Formlet.Render.List as Formlet.Render.List
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Ocelot.Block.Table as Ocelot.Block.Table

render ::
  forall action m render slots.
  MonadAff m =>
  ( { column :: Formlet.Render.List.Key
    , row :: Formlet.Render.List.Key
    } ->
    render action ->
    Halogen.ComponentHTML action slots m
  ) ->
  Formlet.Ocelot.Table.Render render action ->
  Array (Halogen.ComponentHTML action slots m)
render renderElement = case _ of
  Formlet.Ocelot.Table.Render { header, rows } ->
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
  ( { column :: Formlet.Render.List.Key
    , row :: Formlet.Render.List.Key
    } ->
    render action ->
    Halogen.ComponentHTML action slots m
  ) ->
  Formlet.Render.List.List (Formlet.Ocelot.Table.RenderRow render) action ->
  Array (Halogen.ComponentHTML action slots m)
renderRows renderElement = case _ of
  Formlet.Render.List.List keyedRenderRows -> do
    keyedRenderRows
      <#> \({ key, render: row }) ->
        renderRow (\{ column } -> renderElement { column, row: key }) row

renderRow ::
  forall action m render slots.
  ( { column :: Formlet.Render.List.Key } ->
    render action ->
    Halogen.ComponentHTML action slots m
  ) ->
  Formlet.Ocelot.Table.RenderRow render action ->
  Halogen.ComponentHTML action slots m
renderRow renderElement = case _ of
  Formlet.Ocelot.Table.RenderRow { columns } -> do
    Ocelot.Block.Table.row_
      (renderColumns renderElement columns)

renderColumns ::
  forall action m render slots.
  ( { column :: Formlet.Render.List.Key } ->
    render action ->
    Halogen.ComponentHTML action slots m
  ) ->
  Formlet.Render.List.List render action ->
  Array (Halogen.ComponentHTML action slots m)
renderColumns renderElement = case _ of
  Formlet.Render.List.List keyedRenders ->
    keyedRenders
      <#> \({ key, render: column }) ->
        Ocelot.Block.Table.cell_
          [ renderElement { column: key } column ]

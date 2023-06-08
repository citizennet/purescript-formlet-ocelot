module Form2.Ocelot.Sequence.Halogen
  ( Slots
  , render
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Form2.Ocelot.Sequence as Form2.Ocelot.Sequence
import Form2.Ocelot.Sequence.Section.Halogen as Form2.Ocelot.Sequence.Section.Halogen
import Form2.Render.List as Form2.Render.List
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Elements.Keyed as Halogen.HTML.Elements.Keyed
import Halogen.HTML.Events as Halogen.HTML.Events
import Ocelot.Block.Icon as Ocelot.Block.Icon
import Ocelot.Button as Ocelot.Button
import Ocelot.HTML.Properties as Ocelot.HTML.Properties

type Slots action slots =
  Form2.Ocelot.Sequence.Section.Halogen.Slots action slots

-- | Renders a `Form2.Ocelot.Sequence.Render` given a way of rendering each individual
-- | `render` element of the sequence.
render ::
  forall action config m render slots.
  MonadAff m =>
  (Form2.Render.List.Key -> { readonly :: Boolean | config } -> render action -> Halogen.ComponentHTML action (Slots action slots) m) ->
  { readonly :: Boolean | config } ->
  Form2.Ocelot.Sequence.Render render action ->
  Array (Halogen.ComponentHTML action (Slots action slots) m)
render renderElement config (Form2.Ocelot.Sequence.Render render') =
  [ let
      positions = Data.Array.range 0 (Data.Array.length (un Form2.Render.List.List render'.sections) - 1)
    in
      Halogen.HTML.Elements.Keyed.div_
        $ Data.Array.mapWithIndex
            ( \index { key, render: section } ->
                Tuple key $
                  Halogen.HTML.slot
                    (Proxy :: Proxy "field")
                    key
                    (Form2.Ocelot.Sequence.Section.Halogen.component renderElement)
                    { borders: render'.borders
                    , config
                    , key
                    , index
                    , label: render'.label
                    , positions
                    , readonly: config.readonly || render'.readonly
                    , removable: render'.removable
                    , section
                    }
                    identity
            )
        $ un Form2.Render.List.List render'.sections
  , if config.readonly || not render'.extensible then
      Halogen.HTML.text ""
    else
      Ocelot.Button.button
        [ Halogen.HTML.Events.onClick \_ -> render'.onAddSection
        ]
        [ Ocelot.Block.Icon.plus_
        , Halogen.HTML.span
            [ Ocelot.HTML.Properties.css "pl-2" ]
            [ Halogen.HTML.text "Add" ]
        ]
  ]

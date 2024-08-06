module Formlet.Ocelot.RadioGroup.Halogen
  ( render
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Foreign.Object as Foreign.Object
import Formlet.Ocelot.RadioGroup as Formlet.Ocelot.RadioGroup
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Events as Halogen.HTML.Events
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.Block.Radio as Ocelot.Block.Radio
import Ocelot.HTML.Properties as Ocelot.HTML.Properties

render ::
  forall action config m render slots.
  (render action -> Halogen.ComponentHTML action slots m) ->
  { readonly :: Boolean | config } ->
  Formlet.Ocelot.RadioGroup.Render render action ->
  Array (Halogen.ComponentHTML action slots m)
render renderElement { readonly } (Formlet.Ocelot.RadioGroup.Render render') =
  [ Halogen.HTML.div
      [ Ocelot.HTML.Properties.css "flex flex-col" ]
      ( ( render'.options # Data.Array.mapWithIndex \index ({ label, onSelect }) ->
            Ocelot.Block.Radio.radio
              [ Ocelot.HTML.Properties.style $ Foreign.Object.fromHomogeneous
                  { "order": show index
                  }
              ]
              [ Halogen.HTML.Events.onClick \_ -> onSelect
              , Halogen.HTML.Properties.checked (render'.value == Just label)
              , Halogen.HTML.Properties.disabled (readonly || render'.readonly)
              ]
              [ Halogen.HTML.text label ]
        )
          <>
            ( case maybeIndex of
                Nothing -> []
                Just index ->
                  [ Halogen.HTML.div
                      [ Ocelot.HTML.Properties.style $ Foreign.Object.fromHomogeneous
                          { "order": show index
                          }
                      ]
                      [ renderElement render'.render ]
                  ]
            )

      )
  ]
  where
  maybeIndex :: Maybe Int
  maybeIndex = Data.Array.findIndex (\x -> Just x.label == render'.value) render'.options

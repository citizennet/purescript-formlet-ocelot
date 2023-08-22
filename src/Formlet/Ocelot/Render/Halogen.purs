module Formlet.Ocelot.Render.Halogen
  ( Slots
  , render
  , render'
  ) where

import CitizenNet.Prelude

import Data.Functor.Variant as Data.Functor.Variant
import Formlet.Field.Halogen as Formlet.Field.Halogen
import Formlet.Ocelot.Array.Halogen as Formlet.Ocelot.Array.Halogen
import Formlet.Ocelot.Checkbox.Halogen as Formlet.Ocelot.Checkbox.Halogen
import Formlet.Ocelot.CheckboxSet.Halogen as Formlet.Ocelot.CheckboxSet.Halogen
import Formlet.Ocelot.DateTime.Halogen as Formlet.Ocelot.DateTime.Halogen
import Formlet.Ocelot.Dropdown.Halogen as Formlet.Ocelot.Dropdown.Halogen
import Formlet.Ocelot.File.Halogen as Formlet.Ocelot.File.Halogen
import Formlet.Ocelot.Radio.Halogen as Formlet.Ocelot.Radio.Halogen
import Formlet.Ocelot.RadioGroup.Halogen as Formlet.Ocelot.RadioGroup.Halogen
import Formlet.Ocelot.Render as Formlet.Ocelot.Render
import Formlet.Ocelot.Sequence.Halogen as Formlet.Ocelot.Sequence.Halogen
import Formlet.Ocelot.Table.Halogen as Formlet.Ocelot.Table.Halogen
import Formlet.Ocelot.TabularSelect.Halogen as Formlet.Ocelot.TabularSelect.Halogen
import Formlet.Ocelot.Tags.Halogen as Formlet.Ocelot.Tags.Halogen
import Formlet.Ocelot.Text.Halogen as Formlet.Ocelot.Text.Halogen
import Formlet.Ocelot.Textarea.Halogen as Formlet.Ocelot.Textarea.Halogen
import Formlet.Ocelot.Toggle.Halogen as Formlet.Ocelot.Toggle.Halogen
import Formlet.Render.Forest.Halogen as Formlet.Render.Forest.Halogen
import Formlet.Render.List as Formlet.Render.List
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Ocelot.Block.FormField as Ocelot.Block.FormField
import Ocelot.HTML.Properties as Ocelot.HTML.Properties
import Type.Row (type (+))
import Unsafe.Coerce as Unsafe.Coerce

type Slots action slots =
  Formlet.Field.Halogen.Slots action
    + Formlet.Ocelot.DateTime.Halogen.Slots
    + Formlet.Ocelot.Dropdown.Halogen.Slots action
    + Formlet.Ocelot.File.Halogen.Slots action
    + Formlet.Ocelot.Sequence.Halogen.Slots action
    + Formlet.Ocelot.Tags.Halogen.Slots action
    + slots

-- | Transform the `Formlet.Ocelot.Render.Forest` functor into Halogen HTML given ways of
-- | handling any extra render cases and options.
render ::
  forall config options renders action slots m.
  MonadAff m =>
  (Formlet.Render.List.Key -> { readonly :: Boolean | config } -> Option options -> Array (Halogen.ComponentHTML action (Slots action slots) m) -> Array (Halogen.ComponentHTML action (Slots action slots) m)) ->
  (Formlet.Render.List.Key -> { readonly :: Boolean | config } -> Data.Functor.Variant.VariantF renders action -> Array (Halogen.ComponentHTML action (Slots action slots) m)) ->
  { readonly :: Boolean | config } ->
  Formlet.Ocelot.Render.Forest options renders action ->
  Halogen.ComponentHTML action (Slots action slots) m
render renderOtherOptions renderOthers config' =
  Formlet.Render.Forest.Halogen.render renderOptions renderElement config'
    <<< un Formlet.Ocelot.Render.Forest
  where
  renderElement ::
    Formlet.Render.List.Key ->
    { readonly :: Boolean | config } ->
    Data.Functor.Variant.VariantF (Formlet.Ocelot.Render.Renders options renders) action ->
    Array (Halogen.ComponentHTML action (Slots action slots) m)
  renderElement key config renders =
    Data.Functor.Variant.onMatch
      { array:
          Formlet.Ocelot.Array.Halogen.render
            ( \itemKey config'' ->
                render renderOtherOptions renderOthers config''
                  <<< Formlet.Ocelot.Render.mapKey (itemKey <> _)
            )
            config
      , checkbox: Formlet.Ocelot.Checkbox.Halogen.render { key } config
      , checkboxSet: Formlet.Ocelot.CheckboxSet.Halogen.render { key } config
      , dateTime: Formlet.Ocelot.DateTime.Halogen.render config
      , dropdown: Formlet.Ocelot.Dropdown.Halogen.render config
      , file: Formlet.Ocelot.File.Halogen.render config
      , radio: Formlet.Ocelot.Radio.Halogen.render { key } config
      , radioGroup: Formlet.Ocelot.RadioGroup.Halogen.render
          ( render renderOtherOptions renderOthers config
              <<< Formlet.Ocelot.Render.mapKey (key <> _)
          )
          { key }
          config
      , sequence:
          Formlet.Ocelot.Sequence.Halogen.render
            ( \sectionKey _ ->
                render renderOtherOptions renderOthers config
                  <<< Formlet.Ocelot.Render.mapKey (sectionKey <> _)
            )
            config
      , table:
          Formlet.Ocelot.Table.Halogen.render
            ( \({ column, row }) ->
                render renderOtherOptions renderOthers config
                  <<< Formlet.Ocelot.Render.mapKey (_ <> "-" <> row <> "-" <> column)
            )
      , tabularSelect: Formlet.Ocelot.TabularSelect.Halogen.render { key } config
      , tags: Formlet.Ocelot.Tags.Halogen.render config
      , text: Formlet.Ocelot.Text.Halogen.render { key } config
      , textarea: Formlet.Ocelot.Textarea.Halogen.render { key } config
      , toggle: Formlet.Ocelot.Toggle.Halogen.render { key } config
      }
      (renderOthers key config)
      renders

  renderOptions ::
    Formlet.Render.List.Key ->
    { readonly :: Boolean | config } ->
    Option (Formlet.Ocelot.Render.Options options) ->
    Array (Halogen.ComponentHTML action (Slots action slots) m) ->
    Halogen.ComponentHTML action (Slots action slots) m
  renderOptions key config options children =
    Halogen.HTML.slot
      (Proxy :: Proxy "field")
      key
      Formlet.Field.Halogen.component
      { errors: options .? { errors: _ }
      , render: renderField key config options children
      }
      identity

  renderField ::
    Formlet.Render.List.Key ->
    { readonly :: Boolean | config } ->
    Option (Formlet.Ocelot.Render.Options options) ->
    Array (Halogen.ComponentHTML action (Slots action slots) m) ->
    Maybe (Array String) ->
    Halogen.ComponentHTML action (Slots action slots) m
  renderField key config options children mErrors =
    let
      mName :: Maybe String
      mName = options .? { name: _ }

      mDescription :: Maybe String
      mDescription = options .? { description: _ }
    in
      case mName, mDescription of
        Nothing, Nothing ->
          Halogen.HTML.div
            [ Ocelot.HTML.Properties.css "w-full"
            ]
            ( renderOtherOptions key config (coerceOptions options) children
                <>
                  [ Halogen.HTML.div
                      [ Ocelot.HTML.Properties.css "block text-red font-medium"
                      ]
                      (renderValidation (fold mErrors))
                  ]
            )
        _, _ ->
          Ocelot.Block.FormField.field_
            { label:
                case mName of
                  Nothing -> Halogen.HTML.text ""
                  Just name ->
                    Halogen.HTML.span_
                      [ Halogen.HTML.text name
                      , if fromMaybe false (options .? { required: _ }) then
                          Halogen.HTML.span
                            [ Ocelot.HTML.Properties.css "text-red" ]
                            [ Halogen.HTML.text "*" ]
                        else
                          Halogen.HTML.text ""
                      ]
            , helpText: foldMap (pure <<< Halogen.HTML.text) mDescription
            , error: renderValidation (fold mErrors)
            , inputId: key
            }
            (renderOtherOptions key config (coerceOptions options) children)
    where
    coerceOptions :: Option (Formlet.Ocelot.Render.Options options) -> Option options
    coerceOptions = Unsafe.Coerce.unsafeCoerce

  renderValidation :: forall p i. Array String -> Array (Halogen.HTML.HTML p i)
  renderValidation = case _ of
    [] -> []
    [ error ] ->
      [ Halogen.HTML.p_ [ Halogen.HTML.text error ]
      ]
    errors ->
      [ Halogen.HTML.ul_
          $ map (Halogen.HTML.li_ <<< pure <<< Halogen.HTML.text)
          $ errors
      ]

-- | Transform the `Formlet.Ocelot.Render.Forest` functor into Halogen HTML given that it
-- | contains no extra render cases and options than the ones present in
-- | `Formlet.Ocelot.Render.Forest` by default.
render' ::
  forall config action slots m.
  MonadAff m =>
  { readonly :: Boolean | config } ->
  Formlet.Ocelot.Render.Forest () () action ->
  Halogen.ComponentHTML action (Slots action slots) m
render' = render (\_ _ _ -> identity) (\_ _ -> Data.Functor.Variant.case_)

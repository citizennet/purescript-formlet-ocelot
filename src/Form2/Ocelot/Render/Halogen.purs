module Form2.Ocelot.Render.Halogen
  ( Slots
  , render
  , render'
  ) where

import CitizenNet.Prelude

import Data.Functor.Variant as Data.Functor.Variant
import Form2.Field.Halogen as Form2.Field.Halogen
import Form2.Ocelot.Array.Halogen as Form2.Ocelot.Array.Halogen
import Form2.Ocelot.Checkbox.Halogen as Form2.Ocelot.Checkbox.Halogen
import Form2.Ocelot.DateTime.Halogen as Form2.Ocelot.DateTime.Halogen
import Form2.Ocelot.Dropdown.Halogen as Form2.Ocelot.Dropdown.Halogen
import Form2.Ocelot.File.Halogen as Form2.Ocelot.File.Halogen
import Form2.Ocelot.Radio.Halogen as Form2.Ocelot.Radio.Halogen
import Form2.Ocelot.Render as Form2.Ocelot.Render
import Form2.Ocelot.Sequence.Halogen as Form2.Ocelot.Sequence.Halogen
import Form2.Ocelot.Table.Halogen as Form2.Ocelot.Table.Halogen
import Form2.Ocelot.TabularSelect.Halogen as Form2.Ocelot.TabularSelect.Halogen
import Form2.Ocelot.Tags.Halogen as Form2.Ocelot.Tags.Halogen
import Form2.Ocelot.Text.Halogen as Form2.Ocelot.Text.Halogen
import Form2.Ocelot.Textarea.Halogen as Form2.Ocelot.Textarea.Halogen
import Form2.Ocelot.Toggle.Halogen as Form2.Ocelot.Toggle.Halogen
import Form2.Render.Forest.Halogen as Form2.Render.Forest.Halogen
import Form2.Render.List as Form2.Render.List
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Ocelot.Block.FormField as Ocelot.Block.FormField
import Ocelot.HTML.Properties as Ocelot.HTML.Properties
import Type.Row (type (+))
import Unsafe.Coerce as Unsafe.Coerce

type Slots action slots =
  Form2.Field.Halogen.Slots action
    + Form2.Ocelot.DateTime.Halogen.Slots
    + Form2.Ocelot.Dropdown.Halogen.Slots action
    + Form2.Ocelot.File.Halogen.Slots action
    + Form2.Ocelot.Sequence.Halogen.Slots action
    + Form2.Ocelot.Tags.Halogen.Slots action
    + slots

-- | Transform the `Form2.Ocelot.Render.Forest` functor into Halogen HTML given ways of
-- | handling any extra render cases and options.
render ::
  forall config options renders action slots m.
  MonadAff m =>
  (Form2.Render.List.Key -> { readonly :: Boolean | config } -> Option options -> Array (Halogen.ComponentHTML action (Slots action slots) m) -> Array (Halogen.ComponentHTML action (Slots action slots) m)) ->
  (Form2.Render.List.Key -> { readonly :: Boolean | config } -> Data.Functor.Variant.VariantF renders action -> Array (Halogen.ComponentHTML action (Slots action slots) m)) ->
  { readonly :: Boolean | config } ->
  Form2.Ocelot.Render.Forest options renders action ->
  Halogen.ComponentHTML action (Slots action slots) m
render renderOtherOptions renderOthers config' =
  Form2.Render.Forest.Halogen.render renderOptions renderElement config'
    <<< un Form2.Ocelot.Render.Forest
  where
  renderElement ::
    Form2.Render.List.Key ->
    { readonly :: Boolean | config } ->
    Data.Functor.Variant.VariantF (Form2.Ocelot.Render.Renders options renders) action ->
    Array (Halogen.ComponentHTML action (Slots action slots) m)
  renderElement key config renders =
    Data.Functor.Variant.onMatch
      { array:
          Form2.Ocelot.Array.Halogen.render
            ( \itemKey config'' ->
                render renderOtherOptions renderOthers config''
                  <<< Form2.Ocelot.Render.mapKey (itemKey <> _)
            )
            config
      , checkbox: Form2.Ocelot.Checkbox.Halogen.render { key } config
      , dateTime: Form2.Ocelot.DateTime.Halogen.render config
      , dropdown: Form2.Ocelot.Dropdown.Halogen.render config
      , file: Form2.Ocelot.File.Halogen.render config
      , radio: Form2.Ocelot.Radio.Halogen.render { key } config
      , sequence:
          Form2.Ocelot.Sequence.Halogen.render
            ( \sectionKey _ ->
                render renderOtherOptions renderOthers config
                  <<< Form2.Ocelot.Render.mapKey (sectionKey <> _)
            )
            config
      , table:
          Form2.Ocelot.Table.Halogen.render
            ( \{ column, row } ->
                render renderOtherOptions renderOthers config
                  <<< Form2.Ocelot.Render.mapKey (_ <> "-" <> row <> "-" <> column)
            )
      , tabularSelect: Form2.Ocelot.TabularSelect.Halogen.render { key } config
      , tags: Form2.Ocelot.Tags.Halogen.render config
      , text: Form2.Ocelot.Text.Halogen.render { key } config
      , textarea: Form2.Ocelot.Textarea.Halogen.render { key } config
      , toggle: Form2.Ocelot.Toggle.Halogen.render { key } config
      }
      (renderOthers key config)
      renders

  renderOptions ::
    Form2.Render.List.Key ->
    { readonly :: Boolean | config } ->
    Option (Form2.Ocelot.Render.Options options) ->
    Array (Halogen.ComponentHTML action (Slots action slots) m) ->
    Halogen.ComponentHTML action (Slots action slots) m
  renderOptions key config options children =
    Halogen.HTML.slot
      (Proxy :: Proxy "field")
      key
      Form2.Field.Halogen.component
      { errors: options .? { errors: _ }
      , render: renderField key config options children
      }
      identity

  renderField ::
    Form2.Render.List.Key ->
    { readonly :: Boolean | config } ->
    Option (Form2.Ocelot.Render.Options options) ->
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
    coerceOptions :: Option (Form2.Ocelot.Render.Options options) -> Option options
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

-- | Transform the `Form2.Ocelot.Render.Forest` functor into Halogen HTML given that it
-- | contains no extra render cases and options than the ones present in
-- | `Form2.Ocelot.Render.Forest` by default.
render' ::
  forall config action slots m.
  MonadAff m =>
  { readonly :: Boolean | config } ->
  Form2.Ocelot.Render.Forest () () action ->
  Halogen.ComponentHTML action (Slots action slots) m
render' = render (\_ _ _ -> identity) (\_ _ -> Data.Functor.Variant.case_)

module Form2.Ocelot.Sequence.Section.Halogen
  ( Input
  , Query
  , Slot
  , Slots
  , component
  ) where

import CitizenNet.Prelude

import Data.Bifunctor as Data.Bifunctor
import Form2.Field.Halogen as Form2.Field.Halogen
import Form2.Ocelot.Dropdown.Halogen as Form2.Ocelot.Dropdown.Halogen
import Form2.Ocelot.Sequence as Form2.Ocelot.Sequence
import Form2.Render.List as Form2.Render.List
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Events as Halogen.HTML.Events
import Ocelot.Block.Card as Ocelot.Block.Card
import Ocelot.Block.Expandable as Ocelot.Block.Expandable
import Ocelot.Block.Format as Ocelot.Block.Format
import Ocelot.Block.Icon as Ocelot.Block.Icon
import Ocelot.Button as Ocelot.Button
import Ocelot.HTML.Properties as Ocelot.HTML.Properties

data Action config render output
  = FormOutput output
  | Receive (Input config render output)
  | Toggle

type Input config render output =
  { borders :: Boolean
  , config :: config
  , key :: Form2.Render.List.Key
  , index :: Int
  , label :: String
  , positions :: Array Int
  , readonly :: Boolean
  , removable :: Boolean
  , section :: Form2.Ocelot.Sequence.SectionRender render output
  }

type Query =
  Form2.Field.Halogen.Query

-- We reuse `Form2.Field.Halogen.Slot` as the slot type for this component, so
-- that `ClearErrors` and `DisplayErrors` queries to `Form2.Field.Halogen`
-- components can also be dispatched to this component, which we, then, route to
-- the child `Form2.Field.Halogen` components.
--
-- We must do this because this component wraps `Form2.Field.Halogen`
-- components, making them invisible to any parents. If we didn't do this, any
-- `ClearErrors` or `DisplayErrors` queries wouldn't be caught by the child
-- `Form2.Field.Halogen` components.
type Slot output =
  Form2.Field.Halogen.Slot output

type Slots output slots =
  Form2.Field.Halogen.Slots output
    ( position :: Form2.Ocelot.Dropdown.Halogen.Slot Int String
    | slots
    )

type State config render output =
  { collapsed :: Boolean
  , input :: Input config render output
  }

component ::
  forall config m output render slots.
  MonadAff m =>
  (Form2.Render.List.Key -> config -> render output -> Halogen.ComponentHTML output (Slots output slots) m) ->
  Halogen.Component Query (Input config render output) output m
component renderElement =
  Halogen.mkComponent
    { eval:
        Halogen.mkEval
          Halogen.defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            , receive = Just <<< Receive
            }
    , initialState
    , render: render renderElement
    }

handleAction ::
  forall config m output render slots.
  Action config render output ->
  Halogen.HalogenM (State config render output) (Action config render output) (Slots output slots) output m Unit
handleAction = case _ of
  FormOutput output -> Halogen.raise output
  Receive input -> Halogen.modify_ _ { input = input }
  Toggle -> Halogen.modify_ \state -> state { collapsed = not state.collapsed }

handleQuery ::
  forall a config m output render slots.
  Query a ->
  Halogen.HalogenM (State config render output) (Action config render output) (Slots output slots) output m (Maybe a)
handleQuery = case _ of
  Form2.Field.Halogen.ClearErrors done -> do
    _ <- Halogen.queryAll (symbol { field: _ }) (Form2.Field.Halogen.ClearErrors unit)
    pure (Just done)
  Form2.Field.Halogen.DisplayErrors done -> do
    _ <- Halogen.queryAll (symbol { field: _ }) (Form2.Field.Halogen.DisplayErrors unit)
    pure (Just done)
  Form2.Field.Halogen.GetErrors done -> do
    results <- Halogen.queryAll (symbol { field: _ }) (Form2.Field.Halogen.GetErrors identity)
    pure (Just (done (fold results)))

initialState ::
  forall config render output.
  Input config render output ->
  State config render output
initialState input = { collapsed: false, input }

render ::
  forall config m output render slots.
  MonadAff m =>
  (Form2.Render.List.Key -> config -> render output -> Halogen.ComponentHTML output (Slots output slots) m) ->
  State config render output ->
  Halogen.ComponentHTML (Action config render output) (Slots output slots) m
render renderElement state =
  Ocelot.Block.Card.card
    ( if state.input.borders then
        [ Ocelot.HTML.Properties.css "border border-grey-light" ]
      else
        []
    )
    [ renderSectionHeader
    , Data.Bifunctor.bimap (map FormOutput) FormOutput
        $ renderSectionBody
    ]
  where
  renderSectionHeader :: Halogen.ComponentHTML (Action config render output) (Slots output slots) m
  renderSectionHeader =
    let
      Form2.Ocelot.Sequence.SectionRender section = state.input.section
    in
      Halogen.HTML.div
        [ Ocelot.HTML.Properties.css "relative flex border-b border-grey-light mb-4"
        ]
        [ Halogen.HTML.div
            [ Ocelot.HTML.Properties.css "pr-4"
            ]
            [ Ocelot.Block.Expandable.chevron
                (if state.collapsed then Ocelot.Block.Expandable.Collapsed else Ocelot.Block.Expandable.Expanded)
                [ Halogen.HTML.Events.onClick \_ -> Toggle
                , Ocelot.HTML.Properties.css "cursor-pointer"
                ]
            ]
        , Halogen.HTML.div
            [ Ocelot.HTML.Properties.css "text-sm pr-4"
            ]
            [ Halogen.HTML.slot
                (Proxy :: Proxy "position")
                state.input.key
                (Halogen.hoist Halogen.liftAff Form2.Ocelot.Dropdown.Halogen.component)
                { disabled: state.input.readonly
                , items: state.input.positions
                , onChange: identity
                , placeholder: ""
                , render: show <<< (_ + 1)
                , selectedItem: Just state.input.index
                }
                (FormOutput <<< section.onMove)
            ]
        , Ocelot.Block.Format.contentHeading
            [ Ocelot.HTML.Properties.css "flex-1 pr-4" ]
            [ Halogen.HTML.text state.input.label ]
        , if state.input.readonly || not state.input.removable then
            Halogen.HTML.text ""
          else
            Ocelot.Button.buttonClear
              [ Ocelot.HTML.Properties.css "text-right px-0 py-0 h-1"
              , Halogen.HTML.Events.onClick \_ -> FormOutput section.onRemove
              ]
              [ Ocelot.Block.Icon.close_
              ]
        ]

  renderSectionBody :: Halogen.ComponentHTML output (Slots output slots) m
  renderSectionBody =
    let
      Form2.Ocelot.Sequence.SectionRender section = state.input.section
    in
      Ocelot.Block.Expandable.content_
        (if state.collapsed then Ocelot.Block.Expandable.Collapsed else Ocelot.Block.Expandable.Expanded)
        [ Ocelot.Block.Card.card_
            [ renderElement state.input.key state.input.config section.render
            ]
        ]

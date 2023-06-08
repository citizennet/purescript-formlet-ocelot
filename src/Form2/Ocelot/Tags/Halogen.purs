module Form2.Ocelot.Tags.Halogen
  ( Input
  , Query
  , Slot
  , Slots
  , State
  , render
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Foreign.Object as Foreign.Object
import Form2.Ocelot.Tags as Form2.Ocelot.Tags
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Ocelot.Block.ItemContainer as Ocelot.Block.ItemContainer
import Ocelot.Typeahead as Ocelot.Typeahead

type Slots action slots =
  (tags :: Slot action Unit | slots)

render ::
  forall action config m slots.
  MonadAff m =>
  { readonly :: Boolean | config } ->
  Form2.Ocelot.Tags.Render action ->
  Array (Halogen.ComponentHTML action (Slots action slots) m)
render config (Form2.Ocelot.Tags.Render renderSpec) =
  [ Halogen.HTML.slot
      (Proxy :: Proxy "tags")
      unit
      component
      { disabled: config.readonly || renderSpec.readonly
      , items: renderSpec.items
      , minWidth: renderSpec.minWidth
      , onChange: renderSpec.onChange
      , placeholder: renderSpec.placeholder
      , selection: renderSpec.value
      }
      identity
  ]

-----------
-- Internal
-----------
data Action action
  = HandleInitialize
  | HandleOutput (Ocelot.Typeahead.Output Void Array String)
  | HandleReceive (Input action)

type ChildSlots =
  ( tags :: Ocelot.Typeahead.Slot Void Array String Unit
  )

type Input action =
  { disabled :: Boolean
  , items :: Array String
  , minWidth :: Number
  , onChange :: Array String -> action
  , placeholder ::
      { primary :: String
      , secondary :: String
      }
  , selection :: Array String
  }

data Query (a :: Type)

type Slot action = Halogen.Slot Query action

type State action = Input action

-- We define a component that wraps the `Ocelot.Typeahead` component here so its
-- easier to deal with, and so that we can erase `item` from the exported
-- `Slots`.
component ::
  forall action m.
  MonadAff m =>
  Halogen.Component Query (Input action) action m
component =
  Halogen.mkComponent
    { eval:
        Halogen.mkEval
          Halogen.defaultEval
            { handleAction = handleAction
            , initialize = Just HandleInitialize
            , receive = Just <<< HandleReceive
            }
    , initialState: identity
    , render: render'
    }
  where
  handleAction ::
    Action action ->
    Halogen.HalogenM (State action) (Action action) ChildSlots action m Unit
  handleAction = case _ of
    HandleInitialize -> handleInitialize
    HandleOutput output -> case output of
      Ocelot.Typeahead.Searched _ -> pure unit
      Ocelot.Typeahead.Selected _ -> pure unit
      Ocelot.Typeahead.SelectionChanged Ocelot.Typeahead.RemovalQuery selection -> do
        state <- Halogen.get
        Halogen.raise (state.onChange selection)
      Ocelot.Typeahead.SelectionChanged Ocelot.Typeahead.ReplacementQuery _ -> pure unit
      Ocelot.Typeahead.SelectionChanged Ocelot.Typeahead.ResetQuery _ -> pure unit
      Ocelot.Typeahead.SelectionChanged Ocelot.Typeahead.SelectionMessage selection -> do
        state <- Halogen.get
        Halogen.raise (state.onChange selection)
      Ocelot.Typeahead.Emit void -> absurd void
    HandleReceive input -> handleInput input

  handleInitialize :: Halogen.HalogenM (State action) (Action action) ChildSlots action m Unit
  handleInitialize = do
    state <- Halogen.get
    setupMultiTextInput state

  handleInput ::
    Input action ->
    Halogen.HalogenM (State action) (Action action) ChildSlots action m Unit
  handleInput input = do
    Halogen.put input
    setupMultiTextInput input

  setupMultiTextInput ::
    Input action ->
    Halogen.HalogenM (State action) (Action action) ChildSlots action m Unit
  setupMultiTextInput input = do
    old <- Halogen.request (Proxy :: Proxy "tags") unit Ocelot.Typeahead.GetSelected
    when (old /= Just input.selection) do
      void $ Halogen.tell (Proxy :: Proxy "tags") unit (Ocelot.Typeahead.ReplaceSelected input.selection)
    void $ Halogen.tell (Proxy :: Proxy "tags") unit (Ocelot.Typeahead.SetDisabled input.disabled)
    void $ Halogen.tell (Proxy :: Proxy "tags") unit (Ocelot.Typeahead.ReplaceItems (pure input.items))

render' ::
  forall action m.
  MonadAff m =>
  State action ->
  Halogen.ComponentHTML (Action action) ChildSlots m
render' state =
  Halogen.HTML.slot
    (Proxy :: Proxy "tags")
    unit
    ( Halogen.hoist liftAff
        ( Ocelot.Typeahead.component
            { runSelect: flip Data.Array.snoc
            , runRemove: Data.Array.filter <<< (/=)
            , runFilterFuzzy: Ocelot.Typeahead.defFilterFuzzy
            , runFilterItems: Data.Array.difference
            }
        )
    )
    ( ( Ocelot.Typeahead.syncMultiInput
          { renderFuzzy: Halogen.HTML.span_ <<< Ocelot.Block.ItemContainer.boldMatches searchKey
          , itemToObject: Foreign.Object.singleton searchKey
          }
          []
          { minWidth: state.minWidth
          , placeholder: state.placeholder
          }
      )
        { insertable = Ocelot.Typeahead.Insertable identity }
    )
    HandleOutput
  where
  searchKey :: String
  searchKey = "name"

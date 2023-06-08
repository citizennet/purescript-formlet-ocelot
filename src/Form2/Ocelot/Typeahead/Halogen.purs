module Form2.Ocelot.Typeahead.Halogen
  ( HTML
  , Input
  , Query
  , Slot
  , Slots
  , State
  , renderFuzzy
  , renderMulti
  , renderSingle
  ) where

import CitizenNet.Prelude

import DOM.HTML.Indexed as DOM.HTML.Indexed
import Data.Fuzzy as Data.Fuzzy
import Data.Time.Duration as Data.Time.Duration
import Foreign.Object as Foreign.Object
import Form2.Ocelot.Typeahead as Form2.Ocelot.Typeahead
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Network.RemoteData as Network.RemoteData
import Ocelot.Block.ItemContainer as Ocelot.Block.ItemContainer
import Ocelot.Typeahead as Ocelot.Typeahead

type HTML action slots m =
  Halogen.ComponentHTML action (Slots action slots) m

type Slots action slots =
  (typeahead :: Slot action Unit | slots)

-- | Render a fuzzy search result that has been injected into a specific record
-- | key in `toSearchRecord` (see `Form2.Ocelot.Typeahead.async` and
-- | `Form2.Ocelot.Typeahead.async`). This rendering function highlights matched
-- | characters.
renderFuzzy ::
  forall w i a.
  String ->
  Array (Halogen.HTML.IProp DOM.HTML.Indexed.HTMLspan i) ->
  Data.Fuzzy.Fuzzy a ->
  Halogen.HTML.HTML w i
renderFuzzy key props = Halogen.HTML.span props <<< Ocelot.Block.ItemContainer.boldMatches key

-- | Render a multi-selection typeahead, i.e. a typeahead that has `Array` as
-- | its value container type.
renderMulti ::
  forall action config item m slots.
  Eq item =>
  MonadAff m =>
  (forall w i. Data.Fuzzy.Fuzzy item -> Halogen.HTML.HTML w i) ->
  { readonly :: Boolean | config } ->
  Form2.Ocelot.Typeahead.Render Array item action ->
  Array (HTML action slots m)
renderMulti renderFuzzy' { readonly } (Form2.Ocelot.Typeahead.Render render') =
  [ Halogen.HTML.slot
      (Proxy :: Proxy "typeahead")
      unit
      (component (renderMulti' renderFuzzy'))
      { debounceTime: render'.debounceTime
      , disabled: readonly || render'.readonly
      , items: render'.items
      , onChange: render'.onChange
      , placeholder: render'.placeholder
      , selection: render'.value
      , toSearchObject: render'.toSearchObject
      }
      identity
  ]

-- | Render a single-selection typeahead, i.e. a typeahead that has `Maybe` as
-- | its value container type.
renderSingle ::
  forall action config item m slots.
  Eq item =>
  MonadAff m =>
  (forall w i. Data.Fuzzy.Fuzzy item -> Halogen.HTML.HTML w i) ->
  { readonly :: Boolean | config } ->
  Form2.Ocelot.Typeahead.Render Maybe item action ->
  Array (HTML action slots m)
renderSingle renderFuzzy' { readonly } (Form2.Ocelot.Typeahead.Render render') =
  [ Halogen.HTML.slot
      (Proxy :: Proxy "typeahead")
      unit
      (component (renderSingle' renderFuzzy'))
      { debounceTime: render'.debounceTime
      , disabled: readonly || render'.readonly
      , items: render'.items
      , onChange: render'.onChange
      , placeholder: render'.placeholder
      , selection: render'.value
      , toSearchObject: render'.toSearchObject
      }
      identity
  ]

-----------
-- Internal
-----------
data Action container item action
  = HandleInitialize
  | HandleOutput (Ocelot.Typeahead.Output Void container item)
  | HandleReceive (Input container item action)

type ChildSlots container item =
  ( typeahead :: Ocelot.Typeahead.Slot Void container item Unit
  )

type Input container item action =
  { debounceTime :: Maybe Data.Time.Duration.Milliseconds
  , disabled :: Boolean
  , items :: Form2.Ocelot.Typeahead.Items item
  , onChange :: container item -> action
  , placeholder :: String
  , selection :: container item
  , toSearchObject :: item -> Foreign.Object.Object String
  }

data Query (a :: Type)

type Slot action =
  Halogen.Slot Query action

type State container item action =
  Input container item action

-- We define a component that wraps the `Ocelot.Typeahead` component here so its
-- easier to deal with, and so that we can erase `item` from the exported
-- `Slots`.
component ::
  forall action container item m.
  MonadAff m =>
  Eq item =>
  (State container item action -> Halogen.ComponentHTML (Action container item action) (ChildSlots container item) m) ->
  Halogen.Component Query (Input container item action) action m
component render' =
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
    Action container item action ->
    Halogen.HalogenM (State container item action) (Action container item action) (ChildSlots container item) action m Unit
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

  handleInitialize :: Halogen.HalogenM (State container item action) (Action container item action) (ChildSlots container item) action m Unit
  handleInitialize = do
    state <- Halogen.get
    setupTypeahead state

  handleInput ::
    Input container item action ->
    Halogen.HalogenM (State container item action) (Action container item action) (ChildSlots container item) action m Unit
  handleInput input = do
    Halogen.put input
    setupTypeahead input

  setupTypeahead ::
    Input container item action ->
    Halogen.HalogenM (State container item action) (Action container item action) (ChildSlots container item) action m Unit
  setupTypeahead input = do
    void $ Halogen.tell (Proxy :: Proxy "typeahead") unit (Ocelot.Typeahead.ReplaceSelected input.selection)
    void $ Halogen.tell (Proxy :: Proxy "typeahead") unit (Ocelot.Typeahead.SetDisabled input.disabled)
    case input.items of
      Form2.Ocelot.Typeahead.Async _ -> pure unit
      Form2.Ocelot.Typeahead.Sync items -> void $ Halogen.tell (Proxy :: Proxy "typeahead") unit (Ocelot.Typeahead.ReplaceItems items)

renderMulti' ::
  forall action item m.
  Eq item =>
  MonadAff m =>
  (forall w i. Data.Fuzzy.Fuzzy item -> Halogen.HTML.HTML w i) ->
  State Array item action ->
  Halogen.ComponentHTML (Action Array item action) (ChildSlots Array item) m
renderMulti' renderFuzzy' state = case state.items of
  Form2.Ocelot.Typeahead.Async search ->
    Halogen.HTML.slot
      (Proxy :: Proxy "typeahead")
      unit
      (Halogen.hoist liftAff Ocelot.Typeahead.multiHighlightOnly)
      ( Ocelot.Typeahead.asyncMulti
          { async: map Network.RemoteData.fromEither <<< search
          , itemToObject: state.toSearchObject
          , renderFuzzy: renderFuzzy'
          }
          [ Halogen.HTML.Properties.placeholder state.placeholder ]
      )
        { debounceTime = state.debounceTime }
      HandleOutput
  Form2.Ocelot.Typeahead.Sync _ ->
    Halogen.HTML.slot
      (Proxy :: Proxy "typeahead")
      unit
      (Halogen.hoist liftAff Ocelot.Typeahead.multi)
      ( Ocelot.Typeahead.syncMulti
          { itemToObject: state.toSearchObject
          , renderFuzzy: renderFuzzy'
          }
          [ Halogen.HTML.Properties.placeholder state.placeholder ]
      )
        { debounceTime = state.debounceTime }
      HandleOutput

renderSingle' ::
  forall action item m.
  Eq item =>
  MonadAff m =>
  (forall w i. Data.Fuzzy.Fuzzy item -> Halogen.HTML.HTML w i) ->
  State Maybe item action ->
  Halogen.ComponentHTML (Action Maybe item action) (ChildSlots Maybe item) m
renderSingle' renderFuzzy' state = case state.items of
  Form2.Ocelot.Typeahead.Async search ->
    Halogen.HTML.slot
      (Proxy :: Proxy "typeahead")
      unit
      (Halogen.hoist liftAff Ocelot.Typeahead.singleHighlightOnly)
      ( Ocelot.Typeahead.asyncSingle
          { async: map Network.RemoteData.fromEither <<< search
          , itemToObject: state.toSearchObject
          , renderFuzzy: renderFuzzy'
          }
          [ Halogen.HTML.Properties.placeholder state.placeholder ]
      )
        { debounceTime = state.debounceTime }
      HandleOutput
  Form2.Ocelot.Typeahead.Sync _ ->
    Halogen.HTML.slot
      (Proxy :: Proxy "typeahead")
      unit
      (Halogen.hoist liftAff Ocelot.Typeahead.single)
      ( Ocelot.Typeahead.syncSingle
          { itemToObject: state.toSearchObject
          , renderFuzzy: renderFuzzy'
          }
          [ Halogen.HTML.Properties.placeholder state.placeholder ]
      )
        { debounceTime = state.debounceTime }
      HandleOutput

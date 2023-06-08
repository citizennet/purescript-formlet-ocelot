module Form2.Ocelot.Dropdown.Halogen
  ( Query(..)
  , Slot
  , Slots
  , component
  , render
  ) where

import CitizenNet.Prelude

import Form2.Ocelot.Dropdown as Form2.Ocelot.Dropdown
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Ocelot.Button as Ocelot.Button
import Ocelot.Dropdown as Ocelot.Dropdown

type Slots action slots =
  (dropdown :: Slot action Unit | slots)

render ::
  forall slots m config action.
  MonadAff m =>
  { readonly :: Boolean | config } ->
  Form2.Ocelot.Dropdown.Render action ->
  Array (Halogen.ComponentHTML action (Slots action slots) m)
render { readonly } (Form2.Ocelot.Dropdown.Render withRender) =
  [ withRender \render' ->
      Halogen.HTML.slot
        (Proxy :: Proxy "dropdown")
        unit
        (Halogen.hoist liftAff component)
        { disabled: readonly || render'.readonly
        , items: render'.options
        , onChange: render'.onChange
        , placeholder: render'.placeholder
        , render: render'.display
        , selectedItem: map Just render'.value
        }
        identity
  ]

-----------
-- Internal
-----------
data Action item output
  = HandleChange (Ocelot.Dropdown.Output (Item item))
  | HandleReceive (Input item output)

type ChildSlots item =
  ( dropdown :: Ocelot.Dropdown.Slot (Item item) Unit
  )

type Input item output =
  { disabled :: Boolean
  , items :: Array item
  , onChange :: item -> output
  , placeholder :: String
  , render :: item -> String
  , selectedItem :: Maybe item
  }

-- We use this type for the `Ocelot.Dropdown.component` items in order to get
-- around a limitation of that component. It turns out that it does not react to
-- changes to the `render` input, so if the Dropdown's `display` function
-- changes, it would not be picked up by the rendered Halogen component.
type Item item =
  { item :: item
  , label :: String
  }

data Query (a :: Type)

type Slot output =
  Halogen.Slot Query output

type State item output =
  Input item output

component ::
  forall item output.
  Eq item =>
  Halogen.Component Query (Input item output) output Aff
component =
  Halogen.mkComponent
    { eval:
        Halogen.mkEval
          Halogen.defaultEval
            { handleAction = handleAction
            , receive = Just <<< HandleReceive
            }
    , initialState: identity
    , render: render'
    }
  where
  handleAction ::
    Action item output ->
    Halogen.HalogenM (State item output) (Action item output) (ChildSlots item) output Aff Unit
  handleAction = case _ of
    HandleChange output -> handleChange output
    HandleReceive input -> handleReceive input

  handleChange ::
    Ocelot.Dropdown.Output (Item item) ->
    Halogen.HalogenM (State item output) (Action item output) (ChildSlots item) output Aff Unit
  handleChange = case _ of
    Ocelot.Dropdown.Selected { item } -> do
      state <- Halogen.get
      Halogen.raise (state.onChange item)
    Ocelot.Dropdown.VisibilityChanged _ -> pure unit

  handleReceive ::
    Input item output ->
    Halogen.HalogenM (State item output) (Action item output) (ChildSlots item) output Aff Unit
  handleReceive input = do
    Halogen.put input
    let
      toItem :: item -> Item item
      toItem item = { item, label: input.render item }
    void $ Halogen.tell (Proxy :: Proxy "dropdown") unit (Ocelot.Dropdown.SetDisabled input.disabled)
    void $ Halogen.tell (Proxy :: Proxy "dropdown") unit (Ocelot.Dropdown.SetItems (map toItem input.items))
    void $ Halogen.tell (Proxy :: Proxy "dropdown") unit (Ocelot.Dropdown.SetSelection (map toItem input.selectedItem))

  render' ::
    State item output ->
    Halogen.ComponentHTML (Action item output) (ChildSlots item) Aff
  render' state =
    let
      toItem :: item -> Item item
      toItem item = { item, label: state.render item }
    in
      Halogen.HTML.slot
        (Proxy :: Proxy "dropdown")
        unit
        Ocelot.Dropdown.component
        { disabled: state.disabled
        , items: map toItem state.items
        , render:
            Ocelot.Dropdown.defDropdown
              Ocelot.Button.button
              []
              _.label
              state.placeholder
        , selectedItem: map toItem state.selectedItem
        }
        HandleChange

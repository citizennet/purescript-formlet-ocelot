module Formlet.Ocelot.Modal.Halogen
  ( Input
  , Output(..)
  , Query(..)
  , Slot
  , component
  ) where

import CitizenNet.Prelude

import Data.Maybe as Data.Maybe
import Effect.Aff as Effect.Aff
import Effect.Aff.AVar as Effect.Aff.AVar
import Formlet as Formlet
import Formlet.Managed.Halogen as Formlet.Managed.Halogen
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Events as Halogen.HTML.Events
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Ocelot.Button as Ocelot.Button
import Ocelot.HTML.Properties as Ocelot.HTML.Properties
import Ocelot.Part.Modal as Ocelot.Part.Modal
import Option as Option

data Action
  = HandleCancel
  | HandleReceive Input
  | HandleSubmit

type Component config value result m =
  Halogen.Component (Query config value result) Input Output m

type ComponentHTML value result m =
  Halogen.ComponentHTML Action (Slots value result) m

type ComponentM config value result m =
  Halogen.HalogenM (State config value result) Action (Slots value result) Output m

type Input =
  { disabled :: Boolean
  , mainAction :: String
  , title :: String
  }

type Modal config value result =
  { config :: config
  , initialValue :: value
  , result :: Effect.Aff.AVar.AVar (Maybe result)
  }

type Output =
  Void

type Params =
  ( css :: Maybe String
  )

type ParamsOptional =
  ( css :: String
  )

type ParamsRequired =
  () :: Row Type

data Query config value result a
  = ClearErrors a
  | Close a
  | DisplayErrors a
  | GetValue (value -> a)
  | IsOpen (Boolean -> a)
  -- | The `Open` query opens up the modal form with the given config and
  -- | initial value, and blocks the parent component until the user has either
  -- | closed the modal or submitted a valid form, upon which the query returns
  -- | with a valid `result`.
  | Open config value (result -> a)
  | SetValue value a
  | Submit (result -> a)
  | Validate (Either (Array String) result -> a)

type Slot config value result =
  Halogen.Slot (Query config value result) Output

type Slots value result =
  ( form :: Formlet.Managed.Halogen.Slot value result Unit
  )

type State config value result =
  { input :: Input
  , modal :: Maybe (Modal config value result)
  }

closeModal ::
  forall config value result m.
  MonadAff m =>
  ComponentM config value result m Unit
closeModal = do
  state <- Halogen.get
  liftAff
    $ traverse_ (Effect.Aff.AVar.kill (Effect.Aff.error "Modal closed") <<< _.result)
    $ state.modal
  Halogen.modify_ _ { modal = Nothing }

-- | A modal window component that has a rendered `Formlet.Form` as its content.
-- | This component is useful in workflows where one must show a form inside a
-- | modal and wait for the user to fill in and submit the form; see the `Open`
-- | query for more details.
component ::
  forall config m polyParams render result slots value.
  MonadAff m =>
  Functor render =>
  Option.FromRecord polyParams ParamsRequired ParamsOptional =>
  Option.ToRecord ParamsRequired ParamsOptional Params =>
  Formlet.Form config render m value result ->
  (config -> render (m (value -> value)) -> Halogen.ComponentHTML (m (value -> value)) (Formlet.Managed.Halogen.Slots m value slots) m) ->
  Record polyParams ->
  Component config value result m
component form renderForm polyParams =
  Halogen.mkComponent
    { eval:
        Halogen.mkEval
          Halogen.defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            , receive = Just <<< HandleReceive
            }
    , initialState
    , render: render (Formlet.Managed.Halogen.component form renderForm) params
    }
  where
  params :: Record Params
  params =
    Option.recordToRecord
      ( optionRecord polyParams ::
          OptionRecord ParamsRequired ParamsOptional
      )

handleAction ::
  forall config m result state.
  MonadAff m =>
  Action ->
  ComponentM config state result m Unit
handleAction = case _ of
  HandleCancel -> closeModal
  HandleReceive input -> do
    old <- Halogen.get
    when (old.input /= input) do
      Halogen.modify_ _ { input = input }
  HandleSubmit -> void submit

submit ::
  forall config m result state.
  MonadAff m =>
  ComponentM config state result m (Maybe result)
submit = do
  state <- Halogen.get
  if state.input.disabled then
    pure Nothing
  else
    map join $ for state.modal \modal -> do
      mValidated <- Halogen.request (Proxy :: Proxy "form") unit Formlet.Managed.Halogen.Validate
      case mValidated of
        Nothing -> pure Nothing
        Just (Left _) -> pure Nothing -- Validation errors already show up in the form
        Just (Right result) -> do
          _ <- liftAff $ Effect.Aff.AVar.tryPut (Just result) modal.result
          closeModal
          pure (Just result)

handleQuery ::
  forall a config m result value.
  MonadAff m =>
  Query config value result a ->
  ComponentM config value result m (Maybe a)
handleQuery = case _ of
  ClearErrors a -> do
    Just a <$ Halogen.tell (Proxy :: Proxy "form") unit Formlet.Managed.Halogen.ClearErrors
  Close a -> do
    Just a <$ closeModal
  DisplayErrors a -> do
    Just a <$ Halogen.tell (Proxy :: Proxy "form") unit Formlet.Managed.Halogen.DisplayErrors
  GetValue reply -> do
    map reply <$> Halogen.request (Proxy :: Proxy "form") unit Formlet.Managed.Halogen.GetValue
  IsOpen reply -> do
    state <- Halogen.get
    pure $ Just (reply (Data.Maybe.isJust state.modal))
  Open config initialValue reply -> do
    openModal config initialValue
    state <- Halogen.get
    result <- liftAff
      $ traverse (Effect.Aff.AVar.take <<< _.result)
      $ state.modal
    pure $ map reply (join result)
  SetValue value a -> do
    Just a <$ Halogen.tell (Proxy :: Proxy "form") unit (Formlet.Managed.Halogen.SetValue value)
  Submit reply -> do
    map reply <$> submit
  Validate reply -> do
    map reply <$> Halogen.request (Proxy :: Proxy "form") unit Formlet.Managed.Halogen.Validate

initialState ::
  forall config value result.
  Input ->
  State config value result
initialState input =
  { input
  , modal: Nothing
  }

openModal ::
  forall config m result value.
  MonadAff m =>
  config ->
  value ->
  ComponentM config value result m Unit
openModal config initialValue = do
  result <- liftAff Effect.Aff.AVar.empty
  Halogen.modify_ _
    { modal = Just
        { config
        , initialValue
        , result
        }
    }

render ::
  forall config m result value.
  Formlet.Managed.Halogen.Component config m value result ->
  Record Params ->
  State config value result ->
  ComponentHTML value result m
render formComponent params state = case state.modal of
  Nothing -> Halogen.HTML.text ""
  Just modal -> renderModal formComponent params state modal

renderModal ::
  forall config m result value.
  Formlet.Managed.Halogen.Component config m value result ->
  Record Params ->
  State config value result ->
  Modal config value result ->
  ComponentHTML value result m
renderModal formComponent params state modal =
  Ocelot.Part.Modal.modal HandleCancel
    [ Ocelot.HTML.Properties.css $ fromMaybe "" params.css ]
    [ Ocelot.Part.Modal.header
        { buttons:
            [ if state.input.disabled then
                Halogen.HTML.text ""
              else
                Halogen.HTML.a
                  [ Halogen.HTML.Events.onClick \_ -> HandleCancel
                  , Ocelot.HTML.Properties.css "cursor-pointer no-underline text-grey-70 mr-6"
                  ]
                  [ Halogen.HTML.text "Cancel"
                  ]
            , Ocelot.Button.buttonPrimary
                [ if state.input.disabled then
                    Halogen.HTML.Properties.disabled true
                  else
                    Halogen.HTML.Events.onClick \_ -> HandleSubmit
                ]
                [ Halogen.HTML.text state.input.mainAction
                ]
            ]
        , title: [ Halogen.HTML.text state.input.title ]
        }
    , Ocelot.Part.Modal.body
        [ Ocelot.HTML.Properties.css "p-6 overflow-visible" ]
        [ Halogen.HTML.slot_
            (Proxy :: Proxy "form")
            unit
            formComponent
            { config: modal.config
            , initialValue: modal.initialValue
            }
        ]
    ]

module Formlet.Ocelot.DateTime.Halogen
  ( Query(..)
  , Output(..)
  , Slot
  , Slots
  , render
  ) where

import CitizenNet.Prelude

import Data.DateTime as Data.DateTime
import Formlet.Ocelot.DateTime as Formlet.Ocelot.DateTime
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Ocelot.DateTimePicker as Ocelot.DateTimePicker
import Ocelot.HTML.Properties as Ocelot.HTML.Properties
import TimeZone as TimeZone

type Slots slots =
  (dateTimePicker :: Slot Unit | slots)

render ::
  forall slots m config action.
  MonadAff m =>
  { readonly :: Boolean | config } ->
  Formlet.Ocelot.DateTime.Render action ->
  Array (Halogen.ComponentHTML action (Slots slots) m)
render { readonly } (Formlet.Ocelot.DateTime.Render render') =
  [ Halogen.HTML.slot
      (Proxy :: Proxy "dateTimePicker")
      unit
      component
      { disabled: readonly || render'.readonly
      , interval: render'.interval
      , selection: render'.value
      , targetDate: Nothing
      , timezone: render'.timezone
      }
      ( case _ of
          SelectionChanged value -> render'.onChange value
      )
  ]

-----------
-- Internal
-----------
data Action
  = HandleChange Ocelot.DateTimePicker.Output
  | HandleReceive Input

type ChildSlots =
  ( dateTimePicker :: Ocelot.DateTimePicker.Slot Unit
  )

type Input =
  { disabled :: Boolean
  , interval :: Maybe Ocelot.DateTimePicker.Interval
  , selection :: Maybe DateTime
  , targetDate :: Maybe (Data.DateTime.Year /\ Data.DateTime.Month)
  , timezone :: TimeZone.TimeZone
  }

data Output =
  SelectionChanged (Maybe DateTime)

data Query (a :: Type)

type Slot a =
  Halogen.Slot Query Output a

type State =
  Input

component ::
  forall m.
  MonadAff m =>
  Halogen.Component Query Input Output m
component =
  Halogen.mkComponent
    { eval:
        Halogen.mkEval
          $ Halogen.defaultEval
              { handleAction = handleAction
              , receive = Just <<< HandleReceive
              }
    , initialState: identity
    , render: render'
    }
  where
  handleAction ::
    Action ->
    Halogen.HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    HandleChange message -> case message of
      Ocelot.DateTimePicker.DateOutput _ -> pure unit
      Ocelot.DateTimePicker.SelectionChanged selection -> do
        state <- Halogen.get
        Halogen.raise (SelectionChanged (fromLocalDateTime state.timezone <$> selection))
      Ocelot.DateTimePicker.TimeOutput _ -> pure unit
    HandleReceive input -> handleReceive input

  handleReceive ::
    Input ->
    Halogen.HalogenM State Action ChildSlots Output m Unit
  handleReceive input = do
    Halogen.put input
    void $ Halogen.tell (Proxy :: Proxy "dateTimePicker") unit (Ocelot.DateTimePicker.SetDisabled input.disabled)

  render' ::
    State ->
    Halogen.ComponentHTML Action ChildSlots m
  render' state =
    Halogen.HTML.div_
      [ Halogen.HTML.slot
          (Proxy :: Proxy "dateTimePicker")
          unit
          Ocelot.DateTimePicker.component
          { disabled: state.disabled
          , interval: state.interval
          , selection: toLocalDateTime state.timezone state.selection
          , targetDate: state.targetDate
          }
          HandleChange
      , Halogen.HTML.p
          [ Ocelot.HTML.Properties.css "text-right text-grey-50 pt-1"
          ]
          [ Halogen.HTML.text (TimeZone.name state.timezone)
          ]
      ]

-- | Convert from invalid "local" DateTime to valid UTC DateTime.
-- | Since we're only converting by timezone offsets, it should be next
-- | to impossible for the conversion to fail, so it's pretty safe to
-- | swallow any failures here.
fromLocalDateTime ::
  TimeZone.TimeZone ->
  DateTime ->
  DateTime
fromLocalDateTime timezone localDateTime = case TimeZone.adjustFrom timezone localDateTime of
  Nothing -> localDateTime
  Just utcDateTime -> utcDateTime

toLocalDateTime ::
  TimeZone.TimeZone ->
  Maybe DateTime ->
  Maybe DateTime
toLocalDateTime timezone = case _ of
  Nothing -> Nothing
  Just utcDateTime -> TimeZone.adjustTo timezone utcDateTime

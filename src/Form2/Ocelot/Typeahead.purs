module Formlet.Ocelot.Typeahead
  ( Items(..)
  , Render(..)
  , async
  , sync
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Data.Filterable as Data.Filterable
import Data.Foldable as Data.Foldable
import Data.Maybe as Data.Maybe
import Data.Time.Duration as Data.Time.Duration
import Foreign.Object as Foreign.Object
import Formlet as Formlet
import Network.RemoteData as Network.RemoteData
import Option as Option
import Type.Row.Homogeneous as Type.Row.Homogeneous

-- | This is an internal data type used to differentiate between Async and Sync
-- | typeaheads. We could also have defined two separate `Render` types, one for
-- | each typeahead version, but that would have been more work than needed.
data Items item
  = Async (String -> Aff (Either String (Array item)))
  | Sync (Network.RemoteData.RemoteData String (Array item))

type ParamsAsync item record =
  ( debounceTime :: Maybe Data.Time.Duration.Milliseconds
  , placeholder :: Maybe String
  , search :: String -> Aff (Either String (Array item))
  , toSearchRecord :: item -> Record record
  )

type ParamsAsyncOptional =
  ( debounceTime :: Data.Time.Duration.Milliseconds
  , placeholder :: String
  )

type ParamsAsyncRequired item record =
  ( search :: String -> Aff (Either String (Array item))
  , toSearchRecord :: item -> Record record
  )

type ParamsSync item record =
  ( items :: Network.RemoteData.RemoteData String (Array item)
  , placeholder :: Maybe String
  , select :: Maybe (item -> item -> Boolean)
  , toSearchRecord :: item -> Record record
  )

type ParamsSyncOptional item =
  ( placeholder :: String
  , select :: item -> item -> Boolean
  )

type ParamsSyncRequired item record =
  ( items :: Network.RemoteData.RemoteData String (Array item)
  , toSearchRecord :: item -> Record record
  )

newtype Render container item action =
  Render
    { debounceTime :: Maybe Data.Time.Duration.Milliseconds
    , items :: Items item
    , onChange :: container item -> action
    , placeholder :: String
    , readonly :: Boolean
    , toSearchObject :: item -> Foreign.Object.Object String
    , value :: container item
    }

derive instance newtypeRender :: Newtype (Render container item action) _
derive instance functorRender :: Functor (Render container item)

-- | A typeahead Form that gets its set of items with an asynchronous effect
-- | based on a search `String`.
-- |
-- | This form is not automatically injected into a `Formlet.Render.Render`
-- | because the `item` parameter appears on the `Render` type, and fixing a
-- | single label for a typeahead `Render` would prevent us from having multiple
-- | typeaheads with different `item` types in a single form.
async ::
  forall config container item m polyParamsAsync record.
  Functor (Render container item) =>
  Applicative m =>
  Type.Row.Homogeneous.Homogeneous record String =>
  Eq item =>
  Option.FromRecord polyParamsAsync (ParamsAsyncRequired item record) ParamsAsyncOptional =>
  Option.ToRecord (ParamsAsyncRequired item record) ParamsAsyncOptional (ParamsAsync item record) =>
  Record polyParamsAsync ->
  Formlet.Form
    { readonly :: Boolean | config }
    (Render container item)
    m
    (container item)
    (container item)
async polyParamsAsync =
  Formlet.form_ \{ readonly } value ->
    Render
      { debounceTime: paramsAsync.debounceTime
      , items: Async paramsAsync.search
      , onChange: \new -> pure \old -> if readonly then old else new
      , placeholder: fromMaybe "" paramsAsync.placeholder
      , readonly
      , toSearchObject: Foreign.Object.fromHomogeneous <<< paramsAsync.toSearchRecord
      , value
      }
  where
  paramsAsync :: Record (ParamsAsync item record)
  paramsAsync =
    Option.recordToRecord
      ( optionRecord polyParamsAsync ::
          OptionRecord (ParamsAsyncRequired item record) ParamsAsyncOptional
      )

-- | A typeahead Form that has a static set of items to search for.
-- |
-- | This form is not automatically injected into a `Formlet.Render.Render`
-- | because the `item` parameter appears on the `Render` type, and fixing a
-- | single label for a typeahead `Render` would prevent us from having multiple
-- | typeaheads with different `item` types in a single form.
sync ::
  forall config container item polyParamsSync m record.
  Functor (Render container item) =>
  Applicative m =>
  Data.Filterable.Filterable container =>
  Foldable container =>
  Type.Row.Homogeneous.Homogeneous record String =>
  Eq item =>
  Option.FromRecord polyParamsSync (ParamsSyncRequired item record) (ParamsSyncOptional item) =>
  Option.ToRecord (ParamsSyncRequired item record) (ParamsSyncOptional item) (ParamsSync item record) =>
  Record polyParamsSync ->
  Formlet.Form
    { readonly :: Boolean | config }
    (Render container item)
    m
    (container item)
    (container item)
sync polyParamsSync =
  Formlet.form \{ readonly } ->
    { render:
        \value ->
          Render
            { debounceTime: Nothing
            , items: Sync items
            , onChange:
                \new ->
                  pure \old ->
                    if readonly then
                      old
                    else if not Data.Foldable.all (\item -> elemBy (select item) (fold items)) new then
                      old
                    else
                      new
            , placeholder: fromMaybe "" paramsSync.placeholder
            , readonly
            , toSearchObject: Foreign.Object.fromHomogeneous <<< paramsSync.toSearchRecord
            , value:
                case validate value of
                  Left _ -> value
                  Right validated -> validated
            }
    , validate
    }
  where
  elemBy :: forall a. (a -> Boolean) -> Array a -> Boolean
  elemBy predicate = Data.Maybe.isJust <<< Data.Array.find predicate

  paramsSync :: Record (ParamsSync item record)
  paramsSync =
    Option.recordToRecord
      ( optionRecord polyParamsSync ::
          OptionRecord (ParamsSyncRequired item record) (ParamsSyncOptional item)
      )

  items :: Network.RemoteData.RemoteData String (Array item)
  items = paramsSync.items

  select :: item -> item -> Boolean
  select = fromMaybe eq paramsSync.select

  validate :: container item -> Either String (container item)
  validate selection = case items of
    Network.RemoteData.NotAsked -> Left "Please wait for the options to be loaded"
    Network.RemoteData.Loading -> Left "Please wait for the options to be loaded"
    Network.RemoteData.Failure message ->
      Left
        $ "Failed loading the options. Try refreshing this page. If the problem persists, please contact support with the following: "
        <> message
    Network.RemoteData.Success loaded -> do
      Right (Data.Filterable.filterMap (\item -> Data.Array.find (select item) loaded) selection)

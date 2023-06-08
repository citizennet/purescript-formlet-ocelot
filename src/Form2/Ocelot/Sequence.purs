module Formlet.Ocelot.Sequence
  ( Params
  , ParamsOptional
  , ParamsRequired
  , Render(..)
  , SectionRender(..)
  , Sequence
  , fromArray
  , sequence
  , toArray
  ) where

import CitizenNet.Prelude

import Data.Lens.Index as Data.Lens.Index
import Data.Maybe as Data.Maybe
import Formlet as Formlet
import Formlet.Ocelot.KeyedArray as Formlet.Ocelot.KeyedArray
import Formlet.Render as Formlet.Render
import Formlet.Render.List as Formlet.Render.List
import Option as Option

-- | * `defaultValue`
-- |   * initial value for extensible sequence
-- |   * non-extensible if not set with `onAddSection` muted
-- | * `removable`
-- |   * sections are removable by default
-- |   * non-removable if set to `false` with `onRemove` muted
type Params m (value :: Type) =
  ( borders :: Maybe Boolean
  , defaultValue :: Maybe (m value)
  , label :: Maybe String
  , removable :: Maybe Boolean
  )

type ParamsOptional m (value :: Type) =
  ( borders :: Boolean
  , defaultValue :: m value
  , label :: String
  , removable :: Boolean
  )

type ParamsRequired = () :: Row Type

newtype Render render action = Render
  { borders :: Boolean
  , extensible :: Boolean
  , label :: String
  , onAddSection :: action
  , readonly :: Boolean
  , removable :: Boolean
  , sections :: Formlet.Render.List.List (SectionRender render) action
  }

derive instance newtypeRender :: Newtype (Render render action) _

derive instance functorRender :: Functor render => Functor (Render render)

newtype SectionRender render action =
  SectionRender
    { onMove :: Int -> action
    , onRemove :: action
    , render :: render action
    }

derive instance newtypeSectionRender :: Newtype (SectionRender render action) _

derive instance functorSectionRender :: Functor render => Functor (SectionRender render)

type Sequence = Formlet.Ocelot.KeyedArray.KeyedArray

-- | Set up a `sequence` form data from an `Array` with `Formlet.Ocelot.KeyedArray.fromArray`.
fromArray :: forall a. Array a -> Sequence a
fromArray = Formlet.Ocelot.KeyedArray.fromArray

-- | Embed a `Formlet.Form` with `value` and `result` parameters into a sequence
-- | `Form` that edits a `KeyedArray value` and produces an `Array result` as
-- | its validated result.
-- |
-- | This form displays allows for adding new values to the sequence, removing
-- | existing entries and moving them around in the list.
sequence ::
  forall config m options polyParams render renders result value.
  Functor render =>
  Applicative m =>
  Option.FromRecord polyParams ParamsRequired (ParamsOptional m value) =>
  Option.ToRecord ParamsRequired (ParamsOptional m value) (Params m value) =>
  Record polyParams ->
  Formlet.Form { readonly :: Boolean | config } render m value result ->
  Formlet.Form
    { readonly :: Boolean | config }
    (Formlet.Render.Render options (sequence :: Render render | renders))
    m
    (Sequence value)
    (Array result)
sequence polyParams (Formlet.Form f) =
  Formlet.Form \config@{ readonly } ->
    { render:
        \array ->
          Formlet.Render.inj
            { sequence:
                Render
                  { borders: fromMaybe true params.borders
                  , extensible: Data.Maybe.isJust params.defaultValue
                  , label: fromMaybe "" params.label
                  , onAddSection:
                      case params.defaultValue of
                        Nothing -> pure identity
                        Just defaultValue
                          | readonly -> pure identity
                          | otherwise -> Formlet.Ocelot.KeyedArray.snoc <$> defaultValue
                  , readonly
                  , removable
                  , sections:
                      foldMapWithIndex
                        ( \index (Tuple id value) ->
                            Formlet.Render.List.List
                              [ { key: show id
                                , render:
                                    toSectionRender config
                                      { index
                                      , render: (f config).render value
                                      }
                                }
                              ]
                        )
                        $ Formlet.Ocelot.KeyedArray.toArray'
                        $ array
                  }
            }
    , validate: traverse (f config).validate <<< Formlet.Ocelot.KeyedArray.toArray
    }
  where
  params :: Record (Params m value)
  params =
    Option.recordToRecord
      ( optionRecord polyParams ::
          OptionRecord ParamsRequired (ParamsOptional m value)
      )

  removable :: Boolean
  removable = fromMaybe true params.removable

  toSectionRender ::
    forall config'.
    { readonly :: Boolean | config' } ->
    { index :: Int
    , render :: render (m (value -> value))
    } ->
    SectionRender render (m (Sequence value -> Sequence value))
  toSectionRender { readonly } { index, render } =
    SectionRender
      { onMove: pure <<< if readonly then const identity else Formlet.Ocelot.KeyedArray.move index
      , onRemove: pure if readonly || not removable then identity else Formlet.Ocelot.KeyedArray.delete index
      , render:
          map
            (map (if readonly then const identity else Data.Lens.Index.ix index))
            render
      }

-- | Convert the form state of a `sequence` into an `Array`.
toArray :: forall a. Sequence a -> Array a
toArray = Formlet.Ocelot.KeyedArray.toArray

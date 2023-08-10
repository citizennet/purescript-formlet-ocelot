module Formlet.Ocelot.CheckboxSet
  ( Render(..)
  , checkboxSet
  , enumCheckboxSet
  , genericCheckboxSet
  ) where

import CitizenNet.Prelude

import Data.Set as Data.Set
import Formlet as Formlet
import Formlet.Ocelot.Enum as Formlet.Ocelot.Enum
import Formlet.Render as Formlet.Render
import Option as Option
import Record as Record

type Form config m options render a =
  Formlet.Form
    { readonly :: Boolean | config }
    (Formlet.Render.Render options (checkboxSet :: Render | render))
    m
    (Set a)
    (Set a)

newtype Render a =
  Render
    { columns :: Int -- NOTE positive integer (> 0)
    , options ::
        Array
          { checked :: Boolean
          , label :: String
          , onChange :: Boolean -> a
          , readonly :: Boolean
          }
    }

derive instance Newtype (Render a) _
derive instance Functor Render

type Params a =
  ( columns :: Maybe Int
  , display :: a -> String
  , options :: Array a
  )

type ParamsEnum a =
  ( columns :: Maybe Int
  , display :: a -> String
  )

type ParamsOptional =
  ( columns :: Int
  )

type ParamsOptionalEnum =
  ( columns :: Int
  )

type ParamsRequired a =
  ( display :: a -> String
  , options :: Array a
  )

type ParamsRequiredEnum a =
  ( display :: a -> String
  )

-- | A Form that is a set of checkboxes based on the given list of options,
-- | each with its label and value. The result of a `checkboxSet` Form is the
-- | `Set` of all selected options.
checkboxSet ::
  forall a config polyParams m options render.
  Applicative m =>
  Ord a =>
  Option.FromRecord polyParams (ParamsRequired a) ParamsOptional =>
  Option.ToRecord (ParamsRequired a) ParamsOptional (Params a) =>
  Record polyParams ->
  Form config m options render a
checkboxSet polyParams =
  Formlet.form \{ readonly } ->
    { render: \value ->
        Formlet.Render.inj
          { checkboxSet:
              Render
                { columns: fromMaybe 1 params.columns
                , options:
                    params.options
                      # map \a ->
                          let
                            onChange :: Boolean -> m (Set a -> Set a)
                            onChange =
                              if _ then
                                pure (Data.Set.insert a)
                              else
                                pure (Data.Set.delete a)
                          in
                            { checked: Data.Set.member a value
                            , label: params.display a
                            , onChange: if readonly then const (pure identity) else onChange
                            , readonly
                            }
                }
          }
    , validate: Right <<< validate
    }
  where
  params :: Record (Params a)
  params =
    Option.recordToRecord
      ( optionRecord polyParams ::
          OptionRecord (ParamsRequired a) ParamsOptional
      )

  validate :: Set a -> Set a
  validate = Data.Set.intersection (Data.Set.fromFoldable params.options)

-- | A `checkboxSet` Form where all option values are filled in based on the
-- | `Bounded` `Enum` instances for the value type.
enumCheckboxSet ::
  forall a config options m polyParams render.
  Applicative m =>
  Ord a =>
  Bounded a =>
  Enum a =>
  Option.FromRecord polyParams (ParamsRequiredEnum a) ParamsOptionalEnum =>
  Option.ToRecord (ParamsRequiredEnum a) ParamsOptionalEnum (ParamsEnum a) =>
  Record polyParams ->
  Form config m options render a
enumCheckboxSet = checkboxSet <<< addOptions Formlet.Ocelot.Enum.enumOptions

-- | A `checkboxSet` Form where all option values are filled in with the
-- | constructors of a `Generic` enum sum type.
genericCheckboxSet ::
  forall a config options m polyParams render rep.
  Applicative m =>
  Ord a =>
  Generic a rep =>
  Formlet.Ocelot.Enum.GenericEnumOptions a rep =>
  Option.FromRecord polyParams (ParamsRequiredEnum a) ParamsOptionalEnum =>
  Option.ToRecord (ParamsRequiredEnum a) ParamsOptionalEnum (ParamsEnum a) =>
  Record polyParams ->
  Form config m options render a
genericCheckboxSet = checkboxSet <<< addOptions Formlet.Ocelot.Enum.genericEnumOptions

addOptions ::
  forall a polyParams.
  Option.FromRecord polyParams (ParamsRequiredEnum a) ParamsOptionalEnum =>
  Option.ToRecord (ParamsRequiredEnum a) ParamsOptionalEnum (ParamsEnum a) =>
  Array a ->
  Record polyParams ->
  Record (Params a)
addOptions as polyParams = Record.insert (Proxy :: Proxy "options") as params
  where
  params :: Record (ParamsEnum a)
  params = Option.recordToRecord
    ( optionRecord polyParams ::
        OptionRecord (ParamsRequiredEnum a) ParamsOptionalEnum
    )

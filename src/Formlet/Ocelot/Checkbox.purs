module Formlet.Ocelot.Checkbox
  ( Render(..)
  , checkbox
  , checkboxSet
  , checkboxes
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

newtype Render a =
  Render
    { columns :: Array Int -- NOTE positive integer (> 0), we use Array for its Semigroup instance and to collect column values
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

derive newtype instance Semigroup (Render a)

derive newtype instance Monoid (Render a)

type SetParams a =
  ( columns :: Maybe Int
  , display :: a -> String
  , options :: Array a
  )

type SetParamsEnum a =
  ( columns :: Maybe Int
  , display :: a -> String
  )

type SetParamsOptional =
  ( columns :: Int
  )

type SetParamsOptionalEnum =
  ( columns :: Int
  )

type SetParamsRequired a =
  ( display :: a -> String
  , options :: Array a
  )

type SetParamsRequiredEnum a =
  ( display :: a -> String
  )

-- | A singleton checkbox Form that has `Boolean` as its input and output types.
-- | Its render functor is also a `Monoid`, so it can be used in ado-notation.
-- | This should be used in conjunction with `Formlet.Checkbox.checkboxes`, e.g.:
-- |
-- | ```purescript
-- | myCheckboxesForm ::
-- |   forall config m options renders.
-- |   Applicative m =>
-- |   Formlet.Form
-- |     { readonly :: Boolean | config }
-- |     m
-- |     (Formlet.Render.Render options ( checkbox :: Formlet.Checkbox.Render | renders ))
-- |     { foo :: Boolean, bar :: Boolean }
-- |     Boolean
-- | myCheckboxesForm =
-- |   Formlet.Checkbox.checkboxes ado
-- |     foo <-
-- |       Formlet.overRecord { foo: _ }
-- |         $ Formlet.Checkbox.checkbox "Foo"
-- |     bar <-
-- |       Formlet.overRecord { bar: _ }
-- |         $ Formlet.Checkbox.checkbox "Bar"
-- |     in foo || bar
-- | ```
checkbox ::
  forall config m.
  Applicative m =>
  String ->
  Formlet.Form { readonly :: Boolean | config } Render m Boolean Boolean
checkbox label =
  Formlet.form_ \{ readonly } checked ->
    Render
      { columns: []
      , options:
          [ { checked
            , label
            , onChange: if readonly then const (pure identity) else pure <<< const
            , readonly
            }
          ]
      }

-- | A Form that is a set of checkboxes based on the given list of options,
-- | each with its label and value. The result of a `checkboxSet` Form is the
-- | `Set` of all selected options.
checkboxSet ::
  forall config options polyParams renders m a.
  Applicative m =>
  Ord a =>
  Option.FromRecord polyParams (SetParamsRequired a) SetParamsOptional =>
  Option.ToRecord (SetParamsRequired a) SetParamsOptional (SetParams a) =>
  Record polyParams ->
  Formlet.Form { readonly :: Boolean | config } (Formlet.Render.Render options (checkbox :: Render | renders)) m (Set a) (Set a)
checkboxSet polyParams =
  Formlet.form \{ readonly } ->
    { render:
        \value ->
          Formlet.Render.inj
            { checkbox:
                Render
                  { columns: fromMaybe [] (pure <$> params.columns)
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
  params :: Record (SetParams a)
  params =
    Option.recordToRecord
      ( optionRecord polyParams ::
          OptionRecord (SetParamsRequired a) SetParamsOptional
      )

  validate :: Set a -> Set a
  validate = Data.Set.intersection (Data.Set.fromFoldable params.options)

-- | Inject a combination of `checkbox` Forms inside a larger Form structure.
checkboxes ::
  forall config m options renders result value.
  Applicative m =>
  Formlet.Form { readonly :: Boolean | config } Render m value result ->
  Formlet.Form { readonly :: Boolean | config } (Formlet.Render.Render options (checkbox :: Render | renders)) m value result
checkboxes = Formlet.mapRender (Formlet.Render.inj <<< { checkbox: _ })

-- | A `checkboxSet` Form where all option values are filled in based on the
-- | `Bounded` `Enum` instances for the value type.
enumCheckboxSet ::
  forall config options polyParams renders m a.
  Applicative m =>
  Ord a =>
  Bounded a =>
  Enum a =>
  Option.FromRecord polyParams (SetParamsRequiredEnum a) SetParamsOptionalEnum =>
  Option.ToRecord (SetParamsRequiredEnum a) SetParamsOptionalEnum (SetParamsEnum a) =>
  Record polyParams ->
  Formlet.Form { readonly :: Boolean | config } (Formlet.Render.Render options (checkbox :: Render | renders)) m (Set a) (Set a)
enumCheckboxSet = checkboxSet <<< addOptions Formlet.Ocelot.Enum.enumOptions

-- | A `checkboxSet` Form where all option values are filled in with the
-- | constructors of a `Generic` enum sum type.
genericCheckboxSet ::
  forall config options polyParams renders m a rep.
  Applicative m =>
  Ord a =>
  Generic a rep =>
  Formlet.Ocelot.Enum.GenericEnumOptions a rep =>
  Option.FromRecord polyParams (SetParamsRequiredEnum a) SetParamsOptionalEnum =>
  Option.ToRecord (SetParamsRequiredEnum a) SetParamsOptionalEnum (SetParamsEnum a) =>
  Record polyParams ->
  Formlet.Form { readonly :: Boolean | config } (Formlet.Render.Render options (checkbox :: Render | renders)) m (Set a) (Set a)
genericCheckboxSet = checkboxSet <<< addOptions Formlet.Ocelot.Enum.genericEnumOptions

addOptions ::
  forall a polyParams.
  Option.FromRecord polyParams (SetParamsRequiredEnum a) SetParamsOptionalEnum =>
  Option.ToRecord (SetParamsRequiredEnum a) SetParamsOptionalEnum (SetParamsEnum a) =>
  Array a ->
  Record polyParams ->
  Record (SetParams a)
addOptions as polyParams = Record.insert (Proxy :: Proxy "options") as params
  where
  params :: Record (SetParamsEnum a)
  params = Option.recordToRecord
    ( optionRecord polyParams ::
        OptionRecord (SetParamsRequiredEnum a) SetParamsOptionalEnum
    )

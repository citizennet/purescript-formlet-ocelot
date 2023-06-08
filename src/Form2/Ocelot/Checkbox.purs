module Form2.Ocelot.Checkbox
  ( Render(..)
  , checkbox
  , checkboxSet
  , checkboxes
  , enumCheckboxSet
  , genericCheckboxSet
  ) where

import CitizenNet.Prelude

import Data.Set as Data.Set
import Form2 as Form2
import Form2.Ocelot.Enum as Form2.Ocelot.Enum
import Form2.Render as Form2.Render

newtype Render a =
  Render
    ( Array
        { checked :: Boolean
        , label :: String
        , onChange :: Boolean -> a
        , readonly :: Boolean
        }
    )

derive instance Newtype (Render a) _
derive instance Functor Render

derive newtype instance Semigroup (Render a)

derive newtype instance Monoid (Render a)

-- | A singleton checkbox Form that has `Boolean` as its input and output types.
-- | Its render functor is also a `Monoid`, so it can be used in ado-notation.
-- | This should be used in conjunction with `Form2.Checkbox.checkboxes`, e.g.:
-- |
-- | ```purescript
-- | myCheckboxesForm ::
-- |   forall config m options renders.
-- |   Applicative m =>
-- |   Form2.Form
-- |     { readonly :: Boolean | config }
-- |     m
-- |     (Form2.Render.Render options ( checkbox :: Form2.Checkbox.Render | renders ))
-- |     { foo :: Boolean, bar :: Boolean }
-- |     Boolean
-- | myCheckboxesForm =
-- |   Form2.Checkbox.checkboxes ado
-- |     foo <-
-- |       Form2.overRecord { foo: _ }
-- |         $ Form2.Checkbox.checkbox "Foo"
-- |     bar <-
-- |       Form2.overRecord { bar: _ }
-- |         $ Form2.Checkbox.checkbox "Bar"
-- |     in foo || bar
-- | ```
checkbox ::
  forall config m.
  Applicative m =>
  String ->
  Form2.Form { readonly :: Boolean | config } Render m Boolean Boolean
checkbox label =
  Form2.form_ \{ readonly } checked ->
    Render
      [ { checked
        , label
        , onChange: if readonly then const (pure identity) else pure <<< const
        , readonly
        }
      ]

-- | A Form that is a set of checkboxes based on the given list of options,
-- | each with its label and value. The result of a `checkboxSet` Form is the
-- | `Set` of all selected options.
checkboxSet ::
  forall config options renders m a.
  Applicative m =>
  Ord a =>
  { display :: a -> String
  , options :: Array a
  } ->
  Form2.Form { readonly :: Boolean | config } (Form2.Render.Render options (checkbox :: Render | renders)) m (Set a) (Set a)
checkboxSet { display, options } =
  Form2.form \{ readonly } ->
    { render:
        \value ->
          Form2.Render.inj
            { checkbox:
                Render
                  $ options
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
                        , label: display a
                        , onChange: if readonly then const (pure identity) else onChange
                        , readonly
                        }
            }
    , validate: Right <<< validate
    }
  where
  validate :: Set a -> Set a
  validate = Data.Set.intersection (Data.Set.fromFoldable options)

-- | Inject a combination of `checkbox` Forms inside a larger Form structure.
checkboxes ::
  forall config m options renders result value.
  Applicative m =>
  Form2.Form { readonly :: Boolean | config } Render m value result ->
  Form2.Form { readonly :: Boolean | config } (Form2.Render.Render options (checkbox :: Render | renders)) m value result
checkboxes = Form2.mapRender (Form2.Render.inj <<< { checkbox: _ })

-- | A `checkboxSet` Form where all option values are filled in based on the
-- | `Bounded` `Enum` instances for the value type.
enumCheckboxSet ::
  forall config options renders m a.
  Applicative m =>
  Ord a =>
  Bounded a =>
  Enum a =>
  (a -> String) ->
  Form2.Form { readonly :: Boolean | config } (Form2.Render.Render options (checkbox :: Render | renders)) m (Set a) (Set a)
enumCheckboxSet display =
  checkboxSet
    { display
    , options: Form2.Ocelot.Enum.enumOptions
    }

-- | A `checkboxSet` Form where all option values are filled in with the
-- | constructors of a `Generic` enum sum type.
genericCheckboxSet ::
  forall config options renders m a rep.
  Applicative m =>
  Ord a =>
  Generic a rep =>
  Form2.Ocelot.Enum.GenericEnumOptions a rep =>
  (a -> String) ->
  Form2.Form { readonly :: Boolean | config } (Form2.Render.Render options (checkbox :: Render | renders)) m (Set a) (Set a)
genericCheckboxSet display =
  checkboxSet
    { display
    , options: Form2.Ocelot.Enum.genericEnumOptions
    }

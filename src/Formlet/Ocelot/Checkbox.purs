module Formlet.Ocelot.Checkbox
  ( Render(..)
  , checkbox
  , checkboxes
  ) where

import CitizenNet.Prelude

import Formlet as Formlet
import Formlet.Render as Formlet.Render

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
      [ { checked
        , label
        , onChange: if readonly then const (pure identity) else pure <<< const
        , readonly
        }
      ]

-- | Inject a combination of `checkbox` Forms inside a larger Form structure.
checkboxes ::
  forall config m options renders result value.
  Applicative m =>
  Formlet.Form { readonly :: Boolean | config } Render m value result ->
  Formlet.Form { readonly :: Boolean | config } (Formlet.Render.Render options (checkbox :: Render | renders)) m value result
checkboxes = Formlet.mapRender (Formlet.Render.inj <<< { checkbox: _ })

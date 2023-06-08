module Formlet.Ocelot.Dropdown
  ( Render(..)
  , Render'
  , dropdown
  , enumDropdown
  , genericDropdown
  , withRender
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Data.Enum as Data.Enum
import Formlet as Formlet
import Formlet.Ocelot.Enum as Formlet.Ocelot.Enum
import Formlet.Render as Formlet.Render

-- We parametrize the `Render` type over the chosen `item` type but hide it
-- under an existential quantifier. This is so we can use the same `Render`
-- type for multiple different choices of `item`. This representation is
-- equivalent to `exists item. Render' item action`.
newtype Render action =
  Render (forall r. (forall item. Eq item => Render' item action -> r) -> r)

type Render' item action =
  { display :: Maybe item -> String
  , options :: Array (Maybe item)
  , onChange :: Maybe item -> action
  , placeholder :: String
  , readonly :: Boolean
  , value :: Maybe item
  }

instance Functor Render where
  map f (Render withRender') =
    Render \mkRender ->
      withRender' \render ->
        mkRender
          { display: render.display
          , options: render.options
          , onChange: map f render.onChange
          , placeholder: render.placeholder
          , readonly: render.readonly
          , value: render.value
          }

-- | Build a singleton Dropdown Form given a list of options, and a way of
-- | displaying each option.
dropdown ::
  forall config options renders m a.
  Applicative m =>
  Eq a =>
  { display :: a -> String
  , options :: Array a
  , placeholder :: String
  } ->
  Formlet.Form { readonly :: Boolean | config } (Formlet.Render.Render options (dropdown :: Render | renders)) m (Maybe a) (Maybe a)
dropdown { display, options, placeholder } =
  Formlet.form \({ readonly }) ->
    { render:
        \value' ->
          let
            value :: Maybe a
            value = validate value'
          in
            Formlet.Render.inj
              { dropdown:
                  Render \mkRender ->
                    mkRender
                      { display:
                          case value of
                            Nothing -> maybe placeholder display
                            Just _ -> maybe "" display
                      , options: [ Nothing ] <> map Just options
                      , onChange: \a -> pure if readonly then identity else const (validate a)
                      , placeholder
                      , readonly
                      , value
                      }
              }
    , validate: Right <<< validate
    }
  where
  validate :: Maybe a -> Maybe a
  validate = case _ of
    Nothing -> Nothing
    Just a
      | Data.Array.elem a options -> Just a
    Just _ -> Nothing

-- | Build a singleton Dropdown Form whose value type is a `Bounded` `Enum` by
-- | filling in the set of options with all values in the enumeration.
enumDropdown ::
  forall config options renders m a.
  Applicative m =>
  Eq a =>
  Bounded a =>
  Enum a =>
  { display :: a -> String
  , exclude :: Array a
  , placeholder :: String
  } ->
  Formlet.Form { readonly :: Boolean | config } (Formlet.Render.Render options (dropdown :: Render | renders)) m (Maybe a) (Maybe a)
enumDropdown { display, exclude, placeholder } =
  dropdown
    { display
    , options: Data.Array.difference (Data.Enum.enumFromTo bottom top) exclude
    , placeholder
    }

-- | Build a singleton Dropdown Form whose value type is a `Generic` sum type
-- | where all cases have no arguments by filling in the set of options with all
-- | the constructors in the type.
genericDropdown ::
  forall config options renders m a rep.
  Applicative m =>
  Eq a =>
  Generic a rep =>
  Formlet.Ocelot.Enum.GenericEnumOptions a rep =>
  { display :: a -> String
  , placeholder :: String
  } ->
  Formlet.Form { readonly :: Boolean | config } (Formlet.Render.Render options (dropdown :: Render | renders)) m (Maybe a) (Maybe a)
genericDropdown { display, placeholder } =
  dropdown
    { display
    , options: Formlet.Ocelot.Enum.genericEnumOptions
    , placeholder
    }

-- | Unpack the existential quantification in `Render`, allowing us to use the
-- | hidden `item` type parameter as long as it doesn't escape the continuation.
withRender ::
  forall action r.
  Render action ->
  (forall item. Eq item => Render' item action -> r) ->
  r
withRender (Render withRender') = withRender'

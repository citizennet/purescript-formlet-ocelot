module Formlet.Ocelot.Radio
  ( Render(..)
  , enumRadio
  , genericRadio
  , radio
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Formlet as Formlet
import Formlet.Ocelot.Enum as Formlet.Ocelot.Enum
import Formlet.Render as Formlet.Render

newtype Render action =
  Render
    { options ::
        Array
          { label :: String
          , onSelect :: action
          }
    , readonly :: Boolean
    , value :: Maybe String
    }

derive instance Newtype (Render action) _
derive instance Functor Render

-- | A `radio` Form where all option values are filled in based on the `Bounded`
-- | `Enum` instances for the value type.
enumRadio ::
  forall config options renders m a.
  Applicative m =>
  Eq a =>
  Bounded a =>
  Enum a =>
  (a -> String) ->
  Formlet.Form { readonly :: Boolean | config } (Formlet.Render.Render options (radio :: Render | renders)) m (Maybe a) (Maybe a)
enumRadio display =
  radio
    { display
    , options: Formlet.Ocelot.Enum.enumOptions
    }

-- | A `radio` Form where all option values are filled in with the constructors
-- | of a `Generic` enum sum type.
genericRadio ::
  forall config options renders m a rep.
  Applicative m =>
  Eq a =>
  Generic a rep =>
  Formlet.Ocelot.Enum.GenericEnumOptions a rep =>
  (a -> String) ->
  Formlet.Form { readonly :: Boolean | config } (Formlet.Render.Render options (radio :: Render | renders)) m (Maybe a) (Maybe a)
genericRadio display =
  radio
    { display
    , options: Formlet.Ocelot.Enum.genericEnumOptions
    }

-- | Build a singleton Radio Form given a list of options and a way of
-- | displaying them.
radio ::
  forall config options renders m a.
  Applicative m =>
  Eq a =>
  { display :: a -> String
  , options :: Array a
  } ->
  Formlet.Form { readonly :: Boolean | config } (Formlet.Render.Render options (radio :: Render | renders)) m (Maybe a) (Maybe a)
radio { display, options } =
  Formlet.form \{ readonly } ->
    { render:
        \value ->
          Formlet.Render.inj
            { radio:
                Render
                  { options:
                      options
                        <#> \a ->
                          { label: display a
                          , onSelect: if readonly then pure identity else pure (const (Just a))
                          }
                  , readonly
                  , value: map display (validate value)
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

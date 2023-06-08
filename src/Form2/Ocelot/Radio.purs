module Form2.Ocelot.Radio
  ( Render(..)
  , enumRadio
  , genericRadio
  , radio
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Form2 as Form2
import Form2.Ocelot.Enum as Form2.Ocelot.Enum
import Form2.Render as Form2.Render

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
  Form2.Form { readonly :: Boolean | config } (Form2.Render.Render options (radio :: Render | renders)) m (Maybe a) (Maybe a)
enumRadio display =
  radio
    { display
    , options: Form2.Ocelot.Enum.enumOptions
    }

-- | A `radio` Form where all option values are filled in with the constructors
-- | of a `Generic` enum sum type.
genericRadio ::
  forall config options renders m a rep.
  Applicative m =>
  Eq a =>
  Generic a rep =>
  Form2.Ocelot.Enum.GenericEnumOptions a rep =>
  (a -> String) ->
  Form2.Form { readonly :: Boolean | config } (Form2.Render.Render options (radio :: Render | renders)) m (Maybe a) (Maybe a)
genericRadio display =
  radio
    { display
    , options: Form2.Ocelot.Enum.genericEnumOptions
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
  Form2.Form { readonly :: Boolean | config } (Form2.Render.Render options (radio :: Render | renders)) m (Maybe a) (Maybe a)
radio { display, options } =
  Form2.form \{ readonly } ->
    { render:
        \value ->
          Form2.Render.inj
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

module Formlet.Ocelot.RadioGroup
  ( Render(..)
  , enumRadioGroup
  , genericRadioGroup
  , radioGroup
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Formlet as Formlet
import Formlet.Ocelot.Enum as Formlet.Ocelot.Enum
import Formlet.Render as Formlet.Render

newtype Render render action =
  Render
    { options ::
        Array
          { label :: String
          , onSelect :: action
          }
    , readonly :: Boolean
    , render :: render action
    , value :: Maybe String
    }

derive instance Newtype (Render render action) _
derive instance Functor render => Functor (Render render)

type State state a =
  { formState :: state
  , selection :: Maybe a
  }

-- | A `radio` Form where all option values are filled in based on the `Bounded`
-- | `Enum` instances for the value type.
enumRadioGroup ::
  forall config options render renders result m a state.
  Applicative m =>
  Eq a =>
  Bounded a =>
  Enum a =>
  Functor render =>
  (a -> String) ->
  (Maybe a -> Formlet.Form { readonly :: Boolean | config } render m state result) ->
  Formlet.Form { readonly :: Boolean | config } (Formlet.Render.Render options (radioGroup :: Render render | renders)) m (State state a) result
enumRadioGroup display form =
  radioGroup
    { display
    , options: Formlet.Ocelot.Enum.enumOptions
    }
    form

-- | A `radio` Form where all option values are filled in with the constructors
-- | of a `Generic` enum sum type.
genericRadioGroup ::
  forall config options render renders result m a rep state.
  Applicative m =>
  Eq a =>
  Functor render =>
  Generic a rep =>
  Formlet.Ocelot.Enum.GenericEnumOptions a rep =>
  (a -> String) ->
  (Maybe a -> Formlet.Form { readonly :: Boolean | config } render m state result) ->
  Formlet.Form { readonly :: Boolean | config } (Formlet.Render.Render options (radioGroup :: Render render | renders)) m (State state a) result
genericRadioGroup display form =
  radioGroup
    { display
    , options: Formlet.Ocelot.Enum.genericEnumOptions
    }
    form

-- | Build a singleton Radio Form given a list of options and a way of
-- | displaying them.
radioGroup ::
  forall config options render renders result m a state.
  Applicative m =>
  Eq a =>
  Functor render =>
  { display :: a -> String
  , options :: Array a
  } ->
  (Maybe a -> Formlet.Form { readonly :: Boolean | config } render m state result) ->
  Formlet.Form { readonly :: Boolean | config } (Formlet.Render.Render options (radioGroup :: Render render | renders)) m (State state a) result
radioGroup { display, options } toForm =
  Formlet.Form \config ->
    { render:
        \({ formState, selection }) ->
          let
            (Formlet.Form form) = toForm $ validateSelection selection
          in
            Formlet.Render.inj
              { radioGroup:
                  Render
                    { options:
                        options
                          <#> \a ->
                            { label: display a
                            , onSelect: if config.readonly then pure identity else pure (_ { selection = Just a })
                            }
                    , readonly: config.readonly
                    , render: (form config).render formState # map (map \f -> \old -> old { formState = f old.formState })
                    , value: map display (validateSelection selection)
                    }
              }
    , validate: \state ->
        let
          (Formlet.Form form) = toForm $ validateSelection state.selection
        in
          (form config).validate state.formState
    }
  where
  validateSelection :: Maybe a -> Maybe a
  validateSelection = case _ of
    Nothing -> Nothing
    Just a
      | Data.Array.elem a options -> Just a
    Just _ -> Nothing

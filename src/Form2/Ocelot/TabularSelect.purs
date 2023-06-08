module Form2.Ocelot.TabularSelect
  ( Config
  , Form
  , Render(..)
  , enumTabularSelect
  , genericTabularSelect
  , tabularSelect
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Form2 as Form2
import Form2.Ocelot.Enum as Form2.Ocelot.Enum
import Form2.Render as Form2.Render
import Option as Option

type Config config =
  { readonly :: Boolean
  | config
  }

type Form config m options renders =
  Form2.Form (Config config) (Form2.Render.Render options (tabularSelect :: Render | renders)) m

type Params a =
  ( columns :: Int
  , display :: a -> String
  , error :: Maybe (a -> Maybe String)
  , options :: Array a
  )

type ParamsOptional a =
  ( error :: a -> Maybe String
  )

type ParamsRequired a =
  ( columns :: Int
  , display :: a -> String
  , options :: Array a
  )

type ParamsEnum a =
  ( columns :: Int
  , display :: a -> String
  , error :: Maybe (a -> Maybe String)
  )

type ParamsOptionalEnum a =
  ( error :: a -> Maybe String
  )

type ParamsRequiredEnum a =
  ( columns :: Int
  , display :: a -> String
  )

type ParamsGeneric a =
  ( columns :: Int
  , display :: a -> String
  , error :: Maybe (a -> Maybe String)
  )

type ParamsOptionalGeneric a =
  ( error :: a -> Maybe String
  )

type ParamsRequiredGeneric a =
  ( columns :: Int
  , display :: a -> String
  )

newtype Render action =
  Render
    { columns :: Int -- NOTE positive integer (> 0)
    , options ::
        Array
          { error :: Maybe String
          , label :: String
          , onSelect :: action
          }
    , readonly :: Boolean
    , value :: Maybe String
    }

derive instance Newtype (Render action) _
derive instance Functor Render

-- | A `tabularSelect` Form where all option values are filled in based on the `Bounded`
-- | `Enum` instances for the value type.
enumTabularSelect ::
  forall config options polyParams renders m a.
  Applicative m =>
  Eq a =>
  Bounded a =>
  Enum a =>
  Option.FromRecord polyParams (ParamsRequiredEnum a) (ParamsOptionalEnum a) =>
  Option.ToRecord (ParamsRequiredEnum a) (ParamsOptionalEnum a) (ParamsEnum a) =>
  Record polyParams ->
  Form config m options renders (Maybe a) (Maybe a)
enumTabularSelect polyParams =
  tabularSelect
    { display: params.display
    , columns: params.columns
    , error: params.error
    , options
    }
  where
  options :: Array a
  options = Form2.Ocelot.Enum.enumOptions

  params :: Record (ParamsEnum a)
  params =
    Option.recordToRecord
      ( optionRecord polyParams ::
          OptionRecord (ParamsRequiredEnum a) (ParamsOptionalEnum a)
      )

-- | A `tabularSelect` Form where all option values are filled in with the constructors
-- | of a `Generic` enum sum type.
genericTabularSelect ::
  forall config options polyParams renders m a rep.
  Applicative m =>
  Eq a =>
  Generic a rep =>
  Form2.Ocelot.Enum.GenericEnumOptions a rep =>
  Option.FromRecord polyParams (ParamsRequiredGeneric a) (ParamsOptionalGeneric a) =>
  Option.ToRecord (ParamsRequiredGeneric a) (ParamsOptionalGeneric a) (ParamsGeneric a) =>
  Record polyParams ->
  Form config m options renders (Maybe a) (Maybe a)
genericTabularSelect polyParams =
  tabularSelect
    { display: params.display
    , columns: params.columns
    , error: params.error
    , options
    }
  where
  options :: Array a
  options = Form2.Ocelot.Enum.genericEnumOptions

  params :: Record (ParamsGeneric a)
  params =
    Option.recordToRecord
      ( optionRecord polyParams ::
          OptionRecord (ParamsRequiredGeneric a) (ParamsOptionalGeneric a)
      )

-- | Build a singleton TabularSelect Form given a list of options and a way of
-- | displaying them.
tabularSelect ::
  forall config options polyParams renders m a.
  Applicative m =>
  Eq a =>
  Option.FromRecord polyParams (ParamsRequired a) (ParamsOptional a) =>
  Option.ToRecord (ParamsRequired a) (ParamsOptional a) (Params a) =>
  Record polyParams ->
  Form config m options renders (Maybe a) (Maybe a)
tabularSelect polyParams =
  Form2.form \({ readonly }) ->
    { render:
        \value ->
          Form2.Render.inj
            { tabularSelect:
                Render
                  { columns: params.columns
                  , options:
                      params.options
                        <#> \a ->
                          { error: params.error >>= (_ $ a)
                          , label: params.display a
                          , onSelect: if readonly then pure identity else pure (const (Just a))
                          }
                  , readonly
                  , value: map params.display (validate value)
                  }
            }
    , validate: Right <<< validate
    }
  where
  params :: Record (Params a)
  params =
    Option.recordToRecord
      ( optionRecord polyParams ::
          OptionRecord (ParamsRequired a) (ParamsOptional a)
      )

  validate :: Maybe a -> Maybe a
  validate = case _ of
    Nothing -> Nothing
    Just a
      | Data.Array.elem a params.options -> Just a
    Just _ -> Nothing

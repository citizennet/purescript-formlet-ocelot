module Formlet.Ocelot.Enum
  ( class GenericEnumOptions
  , class GenericEnumOptionsFail
  , enumOptions
  , genericEnumOptions
  , genericEnumOptions'
  ) where

import CitizenNet.Prelude

import Data.Enum as Data.Enum
import Data.Generic.Rep as Data.Generic.Rep
import Partial.Unsafe as Partial.Unsafe
import Prim.TypeError as Prim.TypeError

-- | Generate an `Array` of all constructors of a `Generic` enum sum type, i.e.
-- | a sum-type whose constructors are all unary.
class GenericEnumOptions (a :: Type) rep where
  genericEnumOptions' :: forall proxy. proxy a -> Array rep

instance genericEnumOptionsSum ::
  ( GenericEnumOptions a left
  , GenericEnumOptions a right
  ) =>
  GenericEnumOptions a (Data.Generic.Rep.Sum left right) where
  genericEnumOptions' a =
    map Data.Generic.Rep.Inl (genericEnumOptions' a :: Array left)
      <> map Data.Generic.Rep.Inr (genericEnumOptions' a :: Array right)

instance GenericEnumOptions a (Data.Generic.Rep.Constructor name Data.Generic.Rep.NoArguments) where
  genericEnumOptions' _ = [ Data.Generic.Rep.Constructor Data.Generic.Rep.NoArguments ]
else instance genericEnumOptionsArguments ::
  GenericEnumOptionsFail a "Only types whose constructors all have no arguments are supported." =>
  GenericEnumOptions a (Data.Generic.Rep.Constructor name args) where
  genericEnumOptions' = Partial.Unsafe.unsafeCrashWith "impossible"

instance genericEnumOptionsProduct ::
  GenericEnumOptionsFail a "Only Sum types are supported." =>
  GenericEnumOptions a (Data.Generic.Rep.Product left right) where
  genericEnumOptions' = Partial.Unsafe.unsafeCrashWith "impossible"

instance genericEnumOptionsNoConstructors ::
  GenericEnumOptionsFail a "Only Sum types with at least one constructor are supported." =>
  GenericEnumOptions a Data.Generic.Rep.NoConstructors where
  genericEnumOptions' = Partial.Unsafe.unsafeCrashWith "impossible"

-- | Helper class used to produce friendlier error messages for
-- | `GenericEnumOptions`.
class GenericEnumOptionsFail (ty :: Type) (reason :: Symbol)

instance genericEnumOptionsFail ::
  Prim.TypeError.Fail
    ( Prim.TypeError.Above
        ( Prim.TypeError.Beside
            ( Prim.TypeError.Beside
                (Prim.TypeError.Text "`GenericEnumOptions` cannot be used with `")
                (Prim.TypeError.Quote ty)
            )
            (Prim.TypeError.Text "`.")
        )
        (Prim.TypeError.Text reason)
    ) =>
  GenericEnumOptionsFail ty reason

-- | Produce a list of all options/values in an enumeration type.
-- | This can be useful for building Forms that let a user select a value among
-- | a set of options.
enumOptions ::
  forall a.
  Enum a =>
  Bounded a =>
  Array a
enumOptions = Data.Enum.enumFromTo bottom top

-- | Produce a list of all options/values in a `Generic` sum data type whose
-- | constructors all have no arguments, as required by `GenericEnumOptions`.
-- | This can be useful for building Forms that let a user select a value among
-- | a set of options.
genericEnumOptions ::
  forall a rep.
  Generic a rep =>
  GenericEnumOptions a rep =>
  Array a
genericEnumOptions = map Data.Generic.Rep.to (genericEnumOptions' (Proxy :: _ a))

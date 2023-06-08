module Formlet.Ocelot.KeyedArray
  ( KeyedArray
  , cons
  , delete
  , fromArray
  , move
  , null
  , snoc
  , toArray
  , toArray'
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Data.Foldable as Data.Foldable
import Data.Lens as Data.Lens
import Data.Lens.Index as Data.Lens.Index
import Data.Lens.Record as Data.Lens.Record
import Data.Traversable as Data.Traversable
import Data.Tuple as Data.Tuple
import Formlet.Initial as Formlet.Initial
import Test.QuickCheck.Arbitrary as Test.QuickCheck.Arbitrary

-- | A `KeyedArray` is an `Array` where each element has an automatically
-- | assigned unique identifier. This unique identifier can be used as a Halogen
-- | slot key to ensure that all elements will be rendered correctly without
-- | duplicated slots issues.
-- |
-- | This data type can be used as the form state type for collection forms.
-- |
-- | All operations over `KeyedArray` should maintain the invariant that all
-- | `id`s are unique.
newtype KeyedArray a =
  KeyedArray
    { lastId :: Int
    , values :: Array (Tuple Int a)
    }

derive instance eqKeyedArray :: Eq a => Eq (KeyedArray a)

instance showKeyedArray :: Show a => Show (KeyedArray a) where
  show (KeyedArray s) = "(KeyedArray " <> show s <> ")"

derive instance functorKeyedArray :: Functor KeyedArray

instance foldableKeyedArray :: Foldable KeyedArray where
  foldr f z (KeyedArray s) = Data.Foldable.foldr (f <<< Data.Tuple.snd) z s.values
  foldl f z (KeyedArray s) = Data.Foldable.foldl (\b -> f b <<< Data.Tuple.snd) z s.values
  foldMap f (KeyedArray s) = Data.Foldable.foldMap (f <<< Data.Tuple.snd) s.values

instance traversableKeyedArray :: Data.Traversable.Traversable KeyedArray where
  sequence = traverse identity
  traverse f (KeyedArray s) =
    map (KeyedArray <<< s { values = _ })
      $ traverse (\(Tuple id a) -> map (Tuple id) (f a))
      $ s.values

instance initialKeyedArray :: Formlet.Initial.Initial (KeyedArray a) where
  initial = KeyedArray { lastId: 0, values: [] }

instance arbitraryKeyedArray ::
  Test.QuickCheck.Arbitrary.Arbitrary a =>
  Test.QuickCheck.Arbitrary.Arbitrary (KeyedArray a) where
  arbitrary = ado
    values <- Test.QuickCheck.Arbitrary.arbitrary
    in
      KeyedArray
        { lastId: Data.Array.length values
        , values: Data.Array.mapWithIndex Tuple values
        }

instance indexKeyedArray :: Data.Lens.Index.Index (KeyedArray a) Int a where
  ix index =
    Data.Lens.lens (\(KeyedArray k) -> k) (\_ -> KeyedArray)
      <<< Data.Lens.Record.prop (symbol { values: _ })
      <<< Data.Lens.Index.ix index
      <<< Data.Lens._2

-- | Append a new section with the specified value to the beginning of a
-- | `KeyedArray`.
cons :: forall a. a -> KeyedArray a -> KeyedArray a
cons a (KeyedArray s) =
  KeyedArray
    s
      { lastId = s.lastId + 1
      , values = Data.Array.cons (Tuple s.lastId a) s.values
      }

-- | Remove the element in the specified index from a `KeyedArray`. If the index
-- | is out of bounds, the original `KeyedArray` is returned.
delete :: forall a. Int -> KeyedArray a -> KeyedArray a
delete index (KeyedArray s) = KeyedArray s { values = fromMaybe s.values (Data.Array.deleteAt index s.values) }

-- | Create a `KeyedArray` from an `Array`, setting the ids to the indices of
-- | the original `Array`.
fromArray :: forall a. Array a -> KeyedArray a
fromArray values =
  KeyedArray
    { lastId: Data.Array.length values
    , values: Data.Array.mapWithIndex Tuple values
    }

-- | Move an element of a `KeyedArray` in the first index to the position described
-- | by the second index. If either one of the indices is out of bounds, the
-- | original `KeyedArray` is returned.
move :: forall a. Int -> Int -> KeyedArray a -> KeyedArray a
move from to (KeyedArray s) =
  KeyedArray
    s
      { values =
          fromMaybe s.values do
            a <- Data.Array.index s.values from
            Data.Array.insertAt to a =<< Data.Array.deleteAt from s.values
      }

-- | Test whether a `KeyedArray` is empty.
null :: forall a. KeyedArray a -> Boolean
null = case _ of
  KeyedArray { values } -> Data.Array.null values

-- | Append a new section with the specified value to the end of a `KeyedArray`.
snoc :: forall a. a -> KeyedArray a -> KeyedArray a
snoc a (KeyedArray s) =
  KeyedArray
    s
      { lastId = s.lastId + 1
      , values = Data.Array.snoc s.values (Tuple s.lastId a)
      }

-- | Convert a `KeyedArray` into an `Array`, discarding any id information.
toArray :: forall a. KeyedArray a -> Array a
toArray = map Data.Tuple.snd <<< toArray'

-- | Convert a `KeyedArray` into an `Array` while keeping all id information.
toArray' :: forall a. KeyedArray a -> Array (Tuple Int a)
toArray' (KeyedArray s) = s.values

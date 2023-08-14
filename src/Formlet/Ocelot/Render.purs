-- | This module contains the definition of the Render functor we expect to use
-- | most commonly in Ocelot static forms.
module Formlet.Ocelot.Render
  ( Forest(..)
  , Options
  , Render
  , Renders
  , Tree
  , leaf
  , mapKey
  , node
  , singleton
  ) where

import CitizenNet.Prelude

import Formlet as Formlet
import Formlet.Ocelot.Array as Formlet.Ocelot.Array
import Formlet.Ocelot.Checkbox as Formlet.Ocelot.Checkbox
import Formlet.Ocelot.CheckboxSet as Formlet.Ocelot.CheckboxSet
import Formlet.Ocelot.DateTime as Formlet.Ocelot.DateTime
import Formlet.Ocelot.Dropdown as Formlet.Ocelot.Dropdown
import Formlet.Ocelot.File as Formlet.Ocelot.File
import Formlet.Ocelot.Radio as Formlet.Ocelot.Radio
import Formlet.Ocelot.Sequence as Formlet.Ocelot.Sequence
import Formlet.Ocelot.Table as Formlet.Ocelot.Table
import Formlet.Ocelot.TabularSelect as Formlet.Ocelot.TabularSelect
import Formlet.Ocelot.Tags as Formlet.Ocelot.Tags
import Formlet.Ocelot.Text as Formlet.Ocelot.Text
import Formlet.Ocelot.Textarea as Formlet.Ocelot.Textarea
import Formlet.Ocelot.Toggle as Formlet.Ocelot.Toggle
import Formlet.Options as Formlet.Options
import Formlet.Render as Formlet.Render
import Formlet.Render.Forest as Formlet.Render.Forest
import Formlet.Render.List as Formlet.Render.List
import Prim.RowList as Prim.RowList

-- We newtype `Formlet.Render.Forest.Forest` instead of directly using it here
-- because some input types need recursive references to the "main" Render
-- functor, and if we used a type alias, we'd end up with an infinite type; so
-- we newtype it to break the recursion.
newtype Forest options renders action =
  Forest (Formlet.Render.Forest.Forest (Options options) (Renders options renders) action)

derive instance newtypeForest :: Newtype (Forest options renders action) _

derive newtype instance semigroupForest :: Semigroup (Forest options renders action)

derive newtype instance monoidForest :: Monoid (Forest options renders action)

derive newtype instance functorForest :: Functor (Forest options renders)

type Options options =
  ( description :: String
  , errors :: Formlet.Errors
  , name :: String
  , required :: Boolean
  | options
  )

type Render options renders =
  Formlet.Render.Render (Options options) (Renders options renders)

type Renders options renders =
  ( array :: Formlet.Ocelot.Array.Render (Forest options renders)
  , checkbox :: Formlet.Ocelot.Checkbox.Render
  , checkboxSet :: Formlet.Ocelot.CheckboxSet.Render
  , dateTime :: Formlet.Ocelot.DateTime.Render
  , dropdown :: Formlet.Ocelot.Dropdown.Render
  , file :: Formlet.Ocelot.File.Render
  , radio :: Formlet.Ocelot.Radio.Render
  , sequence :: Formlet.Ocelot.Sequence.Render (Forest options renders)
  , table :: Formlet.Ocelot.Table.Render (Forest options renders)
  , tabularSelect :: Formlet.Ocelot.TabularSelect.Render
  , tags :: Formlet.Ocelot.Tags.Render
  , text :: Formlet.Ocelot.Text.Render
  , textarea :: Formlet.Ocelot.Textarea.Render
  , toggle :: Formlet.Ocelot.Toggle.Render
  | renders
  )

type Tree options renders =
  Formlet.Render.Forest.Tree (Options options) (Renders options renders)

-- | Embed a singleton Form into a `Tree`-shaped Form while also setting the
-- | specified options. See `Formlet.Render.Forest.leaf` for a usage example.
--
-- We re-export this function here so we're consistent in all use sites,
-- importing only from `Formlet.Ocelot.Render`, instead of having to import
-- `Formlet.Render.Forest` only for the `leaf` combinator.
leaf ::
  forall config options list record renders m value result.
  Prim.RowList.RowToList record list =>
  Formlet.Options.SetOptions list record options (Formlet.Render.Forest.Tree options renders) =>
  Record record ->
  Formlet.Form config (Formlet.Render.Render options renders) m value result ->
  Formlet.Form config (Formlet.Render.Forest.Tree options renders) m value result
leaf = Formlet.Render.Forest.leaf

-- | Recursively apply the given function to all keys in a `Forest`.
mapKey ::
  forall options renders.
  (Formlet.Render.List.Key -> Formlet.Render.List.Key) ->
  Forest options renders ~> Forest options renders
mapKey f =
  Forest
    <<< Formlet.Render.Forest.mapKey f
    <<< un Forest

-- | Embed a `Forest`-shaped Form into a `Tree`-shaped Form as a anode in the
-- | Tree while also setting the specified options.
-- | See `Formlet.Render.Forest.leaf` for a similar usage example.
node ::
  forall config options list record renders m value result.
  Prim.RowList.RowToList record list =>
  Formlet.Options.SetOptions list record (Options options) (Formlet.Render.Forest.Tree (Options options) (Renders options renders)) =>
  Record record ->
  Formlet.Form config (Forest options renders) m value result ->
  Formlet.Form config (Formlet.Render.Forest.Tree (Options options) (Renders options renders)) m value result
node record = Formlet.Render.Forest.node record <<< Formlet.mapRender (un Forest)

-- | Embed a singleton `Tree`-shaped Form into a `Forest`-shaped form so that it
-- | may be combined with other forms.
singleton ::
  forall config options renders m value result.
  Formlet.Render.List.Key ->
  Formlet.Form config (Formlet.Render.Forest.Tree (Options options) (Renders options renders)) m value result ->
  Formlet.Form config (Forest options renders) m value result
singleton key = Formlet.mapRender Forest <<< Formlet.Render.List.singleton key

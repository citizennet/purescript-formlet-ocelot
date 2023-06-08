-- | This module contains the definition of the Render functor we expect to use
-- | most commonly in Ocelot static forms.
module Form2.Ocelot.Render
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

import Form2 as Form2
import Form2.Ocelot.Array as Form2.Ocelot.Array
import Form2.Ocelot.Checkbox as Form2.Ocelot.Checkbox
import Form2.Ocelot.DateTime as Form2.Ocelot.DateTime
import Form2.Ocelot.Dropdown as Form2.Ocelot.Dropdown
import Form2.Ocelot.File as Form2.Ocelot.File
import Form2.Ocelot.Radio as Form2.Ocelot.Radio
import Form2.Ocelot.Sequence as Form2.Ocelot.Sequence
import Form2.Ocelot.Table as Form2.Ocelot.Table
import Form2.Ocelot.TabularSelect as Form2.Ocelot.TabularSelect
import Form2.Ocelot.Tags as Form2.Ocelot.Tags
import Form2.Ocelot.Text as Form2.Ocelot.Text
import Form2.Ocelot.Textarea as Form2.Ocelot.Textarea
import Form2.Ocelot.Toggle as Form2.Ocelot.Toggle
import Form2.Options as Form2.Options
import Form2.Render as Form2.Render
import Form2.Render.Forest as Form2.Render.Forest
import Form2.Render.List as Form2.Render.List
import Prim.RowList as Prim.RowList

-- We newtype `Form2.Render.Forest.Forest` instead of directly using it here
-- because some input types need recursive references to the "main" Render
-- functor, and if we used a type alias, we'd end up with an infinite type; so
-- we newtype it to break the recursion.
newtype Forest options renders action =
  Forest (Form2.Render.Forest.Forest (Options options) (Renders options renders) action)

derive instance newtypeForest :: Newtype (Forest options renders action) _

derive newtype instance semigroupForest :: Semigroup (Forest options renders action)

derive newtype instance monoidForest :: Monoid (Forest options renders action)

derive newtype instance functorForest :: Functor (Forest options renders)

type Options options =
  ( description :: String
  , errors :: Form2.Errors
  , name :: String
  , required :: Boolean
  | options
  )

type Render options renders =
  Form2.Render.Render (Options options) (Renders options renders)

type Renders options renders =
  ( array :: Form2.Ocelot.Array.Render (Forest options renders)
  , checkbox :: Form2.Ocelot.Checkbox.Render
  , dateTime :: Form2.Ocelot.DateTime.Render
  , dropdown :: Form2.Ocelot.Dropdown.Render
  , file :: Form2.Ocelot.File.Render
  , radio :: Form2.Ocelot.Radio.Render
  , sequence :: Form2.Ocelot.Sequence.Render (Forest options renders)
  , table :: Form2.Ocelot.Table.Render (Forest options renders)
  , tabularSelect :: Form2.Ocelot.TabularSelect.Render
  , tags :: Form2.Ocelot.Tags.Render
  , text :: Form2.Ocelot.Text.Render
  , textarea :: Form2.Ocelot.Textarea.Render
  , toggle :: Form2.Ocelot.Toggle.Render
  | renders
  )

type Tree options renders =
  Form2.Render.Forest.Tree (Options options) (Renders options renders)

-- | Embed a singleton Form into a `Tree`-shaped Form while also setting the
-- | specified options. See `Form2.Render.Forest.leaf` for a usage example.
--
-- We re-export this function here so we're consistent in all use sites,
-- importing only from `Form2.Ocelot.Render`, instead of having to import
-- `Form2.Render.Forest` only for the `leaf` combinator.
leaf ::
  forall config options list record renders m value result.
  Prim.RowList.RowToList record list =>
  Form2.Options.SetOptions list record options (Form2.Render.Forest.Tree options renders) =>
  Record record ->
  Form2.Form config (Form2.Render.Render options renders) m value result ->
  Form2.Form config (Form2.Render.Forest.Tree options renders) m value result
leaf = Form2.Render.Forest.leaf

-- | Recursively apply the given function to all keys in a `Forest`.
mapKey ::
  forall options renders.
  (Form2.Render.List.Key -> Form2.Render.List.Key) ->
  Forest options renders ~> Forest options renders
mapKey f =
  Forest
    <<< Form2.Render.Forest.mapKey f
    <<< un Forest

-- | Embed a `Forest`-shaped Form into a `Tree`-shaped Form as a anode in the
-- | Tree while also setting the specified options.
-- | See `Form2.Render.Forest.leaf` for a similar usage example.
node ::
  forall config options list record renders m value result.
  Prim.RowList.RowToList record list =>
  Form2.Options.SetOptions list record (Options options) (Form2.Render.Forest.Tree (Options options) (Renders options renders)) =>
  Record record ->
  Form2.Form config (Forest options renders) m value result ->
  Form2.Form config (Form2.Render.Forest.Tree (Options options) (Renders options renders)) m value result
node record = Form2.Render.Forest.node record <<< Form2.mapRender (un Forest)

-- | Embed a singleton `Tree`-shaped Form into a `Forest`-shaped form so that it
-- | may be combined with other forms.
singleton ::
  forall config options renders m value result.
  Form2.Render.List.Key ->
  Form2.Form config (Form2.Render.Forest.Tree (Options options) (Renders options renders)) m value result ->
  Form2.Form config (Forest options renders) m value result
singleton key = Form2.mapRender Forest <<< Form2.Render.List.singleton key

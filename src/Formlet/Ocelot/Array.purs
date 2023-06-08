module Formlet.Ocelot.Array
  ( Render(..)
  , RenderRow(..)
  , array
  ) where

import CitizenNet.Prelude

import Data.Lens.Index as Data.Lens.Index
import Formlet as Formlet
import Formlet.Ocelot.KeyedArray as Formlet.Ocelot.KeyedArray
import Formlet.Render as Formlet.Render
import Formlet.Render.List as Formlet.Render.List

newtype Render render action =
  Render
    { onAddSection :: Maybe action
    , rows :: Formlet.Render.List.List (RenderRow render) action
    }

derive instance newtypeRender :: Newtype (Render render action) _
derive instance functorRender :: Functor render => Functor (Render render)

newtype RenderRow render (action :: Type) =
  RenderRow
    { onRemove :: Maybe action
    , render :: render action
    }

derive instance newtypeRenderRow :: Newtype (RenderRow render action) _
derive instance functorRenderRow :: Functor render => Functor (RenderRow render)

-- | Embed a `Formlet.Form` with `value` and `result` parameters into a sequence
-- | `Form` that edits a `KeyedArray value` and produces an `Array result` as
-- | its validated result.
-- |
-- | Unlike `Formlet.Ocelot.Sequence`, this form does not display labels or
-- | sequence numbers, and does not allow moving items in the list.
array ::
  forall config m options render renders result value.
  Applicative m =>
  Functor render =>
  { defaultValue :: Maybe value
  , form :: Formlet.Form { readonly :: Boolean | config } render m value result
  } ->
  Formlet.Form
    { readonly :: Boolean | config }
    (Formlet.Render.Render options (array :: Render render | renders))
    m
    (Formlet.Ocelot.KeyedArray.KeyedArray value)
    (Array result)
array { defaultValue, form } =
  Formlet.Form \config@{ readonly } ->
    { render: \values ->
        Formlet.Render.inj
          { array:
              Render
                { onAddSection: pure <<< Formlet.Ocelot.KeyedArray.snoc <$> defaultValue
                , rows:
                    foldMapWithIndex
                      ( \index (Tuple id value) ->
                          Formlet.Render.List.List
                            [ { key: show id
                              , render:
                                  RenderRow
                                    { onRemove:
                                        if readonly then
                                          Nothing
                                        else
                                          Just $ pure (Formlet.Ocelot.KeyedArray.delete index)
                                    , render:
                                        map (map (if readonly then const identity else Data.Lens.Index.ix index))
                                          $ (un Formlet.Form form config).render
                                          $ value
                                    }
                              }
                            ]
                      )
                      $ Formlet.Ocelot.KeyedArray.toArray'
                      $ values
                }
          }
    , validate: traverse (un Formlet.Form form config).validate <<< Formlet.Ocelot.KeyedArray.toArray
    }

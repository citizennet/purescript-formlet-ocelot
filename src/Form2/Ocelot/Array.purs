module Form2.Ocelot.Array
  ( Render(..)
  , RenderRow(..)
  , array
  ) where

import CitizenNet.Prelude

import Data.Lens.Index as Data.Lens.Index
import Form2 as Form2
import Form2.Ocelot.KeyedArray as Form2.Ocelot.KeyedArray
import Form2.Render as Form2.Render
import Form2.Render.List as Form2.Render.List

newtype Render render action =
  Render
    { onAddSection :: Maybe action
    , rows :: Form2.Render.List.List (RenderRow render) action
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

-- | Embed a `Form2.Form` with `value` and `result` parameters into a sequence
-- | `Form` that edits a `KeyedArray value` and produces an `Array result` as
-- | its validated result.
-- |
-- | Unlike `Form2.Ocelot.Sequence`, this form does not display labels or
-- | sequence numbers, and does not allow moving items in the list.
array ::
  forall config m options render renders result value.
  Applicative m =>
  Functor render =>
  { defaultValue :: Maybe value
  , form :: Form2.Form { readonly :: Boolean | config } render m value result
  } ->
  Form2.Form
    { readonly :: Boolean | config }
    (Form2.Render.Render options (array :: Render render | renders))
    m
    (Form2.Ocelot.KeyedArray.KeyedArray value)
    (Array result)
array { defaultValue, form } =
  Form2.Form \config@{ readonly } ->
    { render: \values ->
        Form2.Render.inj
          { array:
              Render
                { onAddSection: pure <<< Form2.Ocelot.KeyedArray.snoc <$> defaultValue
                , rows:
                    foldMapWithIndex
                      ( \index (Tuple id value) ->
                          Form2.Render.List.List
                            [ { key: show id
                              , render:
                                  RenderRow
                                    { onRemove:
                                        if readonly then
                                          Nothing
                                        else
                                          Just $ pure (Form2.Ocelot.KeyedArray.delete index)
                                    , render:
                                        map (map (if readonly then const identity else Data.Lens.Index.ix index))
                                          $ (un Form2.Form form config).render
                                          $ value
                                    }
                              }
                            ]
                      )
                      $ Form2.Ocelot.KeyedArray.toArray'
                      $ values
                }
          }
    , validate: traverse (un Form2.Form form config).validate <<< Form2.Ocelot.KeyedArray.toArray
    }

module Formlet.Ocelot.Table
  ( Render(..)
  , RenderRow(..)
  , TableForm(..)
  , column
  , table
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Data.Lens.Index as Data.Lens.Index
import Data.Validation.Semigroup as Data.Validation.Semigroup
import Formlet as Formlet
import Formlet.Ocelot.KeyedArray as Formlet.Ocelot.KeyedArray
import Formlet.Render as Formlet.Render
import Formlet.Render.List as Formlet.Render.List

-- | A rendered Table form is represented as an array of column titles for the
-- | header and a list of rows of rendered cells.
-- |
-- | This represents a table with a fixed number of rows. If we ever want to add
-- | support for adding or removing rows from the table, it would be simply a
-- | matter of changing this type and `RenderRow`.
newtype Render (render :: Type -> Type) action =
  Render
    { header :: Array String
    , rows :: Formlet.Render.List.List (RenderRow render) action
    }

derive instance newtypeRender :: Newtype (Render render action) _

derive instance functorRender :: Functor render => Functor (Render render)

derive newtype instance semigroupRender :: Semigroup (Render render action)

derive newtype instance monoidRender :: Monoid (Render render action)

newtype RenderRow (render :: Type -> Type) action =
  RenderRow
    { columns :: Formlet.Render.List.List render action
    }

derive instance newtypeRenderRow :: Newtype (RenderRow render action) _

derive instance functorRenderRow :: Functor render => Functor (RenderRow render)

-- | A mini-DSL for specifying a table form as a collection of columns and a
-- | validation function. See the documentation on `table` for an example.
-- |
-- | This is very similar to the definition of `Formlet.Form`, except for the
-- | fact that the columns and their `title`s are baked into the form itself.
--
-- We can't represent this with `Formlet.Form` and a specific render functor
-- that contains the column titles, since that would require passing the value
-- of a single row to the column forms in order to get the title, but that's not
-- possible as we need the column titles before traversing the rows.
newtype TableForm config render (m :: Type -> Type) row result =
  TableForm
    ( config ->
      { columns ::
          Array
            { render :: row -> render (m (row -> row))
            , title :: String
            }
      , validate :: row -> Data.Validation.Semigroup.V Formlet.Errors result
      }
    )

derive instance newtypeTableForm :: Newtype (TableForm config render m row result) _

derive instance functorTableForm :: Functor (TableForm config render m row)

instance applyTableForm :: Semigroup (render (m (row -> row))) => Apply (TableForm config render m row) where
  apply (TableForm ff) (TableForm fa) =
    TableForm \config ->
      let
        { columns: columns1, validate: mf } = ff config

        { columns: columns2, validate: ma } = fa config
      in
        { columns: columns1 <> columns2
        , validate: \row -> mf row <*> ma row
        }

instance applicativeTableForm :: Monoid (render (m (row -> row))) => Applicative (TableForm config render m row) where
  pure a = TableForm \_ -> { columns: mempty, validate: \_ -> pure a }

-- | Specify a column of a `TableForm` as a column title and a `Formlet.Form` that
-- | will be used to render and validate each of the column's rows.
column ::
  forall config m render result row.
  Applicative m =>
  String ->
  Formlet.Form config render m row result ->
  TableForm config render m row result
column title form =
  TableForm \config ->
    let
      { render, validate } = un Formlet.Form form config
    in
      { columns:
          [ { render
            , title
            }
          ]
      , validate
      }

-- | Build a table `Formlet.Form` from a `TableForm` specification.
-- |
-- | The `TableForm` specification represents how to render and validate a
-- | single row. The `table` function, then transforms that into a form that
-- | edits multiple rows, e.g.:
-- |
-- | ```purescript
-- | userTable ::
-- |   Formlet.Form _ _ _
-- |     (Array { fullName :: String, username :: String, active :: Boolean })
-- |     (NonEmptyArray { username :: NonEmptyString, active :: Boolean })
-- | userTable =
-- |   Formlet.Render.OMS.leaf { name: "User Table" }
-- |     $ Formlet.Validation.validate (Formlet.Validation.isNonEmptyArray "User Table")
-- |     $ Formlet.Table.table ado
-- |         Formlet.Table.column "Full Name"
-- |           $ Formlet.Halogen.HTML.htmlWithValue \row ->
-- |               Halogen.HTML.text row.fullName
-- |         username <-
-- |           Formlet.Table.column "Username"
-- |             $ Formlet.overRecord { username: _ }
-- |             $ Formlet.Validation.validated (Formlet.Validation.isNonEmptyString { name: "Username" })
-- |             $ Formlet.Text.text {}
-- |         active <-
-- |           Formlet.Table.column "Active"
-- |             $ Formlet.overRecord { active: _ }
-- |             $ Formlet.Toggle.toggle
-- |         in { active, username }
-- | ```
table ::
  forall config m options render renders result row.
  Applicative m =>
  Functor render =>
  TableForm config render m row result ->
  Formlet.Form
    config
    (Formlet.Render.Render options (table :: Render render | renders))
    m
    (Formlet.Ocelot.KeyedArray.KeyedArray row)
    (Array result)
table tableSpec =
  Formlet.form \config ->
    let
      { columns, validate } = un TableForm tableSpec config
    in
      { render:
          \rows ->
            Formlet.Render.inj
              { table:
                  Render
                    { header: columns <#> _.title
                    , rows:
                        Formlet.Render.List.List
                          $ Formlet.Ocelot.KeyedArray.toArray' rows
                          # Data.Array.mapWithIndex \index (Tuple id row) ->
                              { key: show id
                              , render:
                                  RenderRow
                                    { columns:
                                        Formlet.Render.List.List
                                          $ columns
                                          # map \columnSpec ->
                                              { key: columnSpec.title
                                              , render:
                                                  map (map (Data.Lens.Index.ix index))
                                                    $ columnSpec.render
                                                    $ row
                                              }
                                    }
                              }
                    }
              }
      , validate:
          -- We pick the only the first error of the table here to summarize
          -- the table validation. The errors in each individual cell will
          -- already be visible through the `errors` render option.
          -- XXX: it looks like `Formlet.Errors` should be `NonEmptyArray String`
          -- instead of `Array String`.
          lmap (fromMaybe "" <<< Data.Array.head)
            <<< Data.Validation.Semigroup.toEither
            <<< traverse validate
            <<< Formlet.Ocelot.KeyedArray.toArray
      }

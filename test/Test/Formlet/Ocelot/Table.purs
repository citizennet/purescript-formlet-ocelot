module Test.Formlet.Ocelot.Table
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Data.Const as Data.Const
import Data.Foldable as Data.Foldable
import Data.Identity as Data.Identity
import Data.String as Data.String
import Formlet as Formlet
import Formlet.Ocelot.KeyedArray as Formlet.Ocelot.KeyedArray
import Formlet.Ocelot.Table as Formlet.Ocelot.Table
import Formlet.Render as Formlet.Render
import Formlet.Render.List as Formlet.Render.List
import Test.QuickCheck ((===))
import Test.QuickCheck.Gen as Test.QuickCheck.Gen
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Formlet.Ocelot.Table" do
    Test.Unit.test "`table` should render all column headers" do
      Test.Unit.QuickCheck.quickCheck \columns ->
        let
          rendered :: Formlet.Ocelot.Table.Render Data.Identity.Identity (Formlet.Ocelot.KeyedArray.KeyedArray Unit -> Formlet.Ocelot.KeyedArray.KeyedArray Unit)
          rendered =
            Formlet.Render.match { table: map (un Data.Identity.Identity) }
              $ Formlet.render
                  ( Formlet.Ocelot.Table.table
                      $ traverse (\column -> Formlet.Ocelot.Table.column column (pure unit))
                      $ columns
                  )
                  unit
                  (Formlet.Ocelot.KeyedArray.fromArray [])

          expected :: Array String
          expected = columns

          actual :: Array String
          actual = (un Formlet.Ocelot.Table.Render rendered).header
        in
          expected === actual
    Test.Unit.test "`table` should render all rows and all columns" do
      Test.Unit.QuickCheck.quickCheck \columns rows ->
        let
          rendered :: Formlet.Ocelot.Table.Render (Data.Const.Const String) (Formlet.Ocelot.KeyedArray.KeyedArray String -> Formlet.Ocelot.KeyedArray.KeyedArray String)
          rendered =
            Formlet.Render.match { table: map (un Data.Identity.Identity) }
              $ Formlet.render
                  ( Formlet.Ocelot.Table.table
                      $ traverse (\column -> Formlet.Ocelot.Table.column column (prefixForm column))
                      $ columns
                  )
                  unit
                  (Formlet.Ocelot.KeyedArray.fromArray rows)

          expected :: Array (Array String)
          expected = map (\row -> map (_ <> row) columns) rows

          actual :: Array (Array String)
          actual =
            map (map (un Data.Const.Const) <<< Formlet.Render.List.toArray <<< _.columns <<< un Formlet.Ocelot.Table.RenderRow)
              $ Formlet.Render.List.toArray
              $ (un Formlet.Ocelot.Table.Render rendered).rows
        in
          expected === actual
    Test.Unit.test "`table` should correctly route updates to the form state" do
      Test.Unit.QuickCheck.quickCheck \(columns :: NonEmptyArray String) rows newValue -> do
        index <- Test.QuickCheck.Gen.chooseInt 0 (Data.Foldable.length rows)
        let
          rendered :: Formlet.Ocelot.Table.Render Data.Identity.Identity (Formlet.Ocelot.KeyedArray.KeyedArray String -> Formlet.Ocelot.KeyedArray.KeyedArray String)
          rendered =
            Formlet.Render.match { table: map (un Data.Identity.Identity) }
              $ Formlet.render
                  ( Formlet.Ocelot.Table.table
                      $ traverse (\column -> Formlet.Ocelot.Table.column column (identityForm newValue))
                      $ columns
                  )
                  unit
                  rows

          array :: Array String
          array = Formlet.Ocelot.KeyedArray.toArray rows

          expected :: Array String
          expected = fromMaybe array (Data.Array.modifyAt index (const newValue) array)

          actual :: Array String
          actual =
            let
              Formlet.Render.List.List renderedRows = (un Formlet.Ocelot.Table.Render rendered).rows
            in
              fromMaybe array do
                { render: row } <- Data.Array.index renderedRows index
                let
                  Formlet.Ocelot.Table.RenderRow { columns: Formlet.Render.List.List renderedColumns } = row
                -- It doesn't matter which column we pick here as all columns
                -- are editing the same value.
                { render } <- Data.Array.head renderedColumns
                pure $ Formlet.Ocelot.KeyedArray.toArray (un Data.Identity.Identity render rows)
        pure $ expected === actual
    Test.Unit.test "`table` should produce only the first validation error of all cells" do
      Test.Unit.QuickCheck.quickCheck \columns rows ->
        let
          columnForm :: forall config. String -> Formlet.Form config (Data.Const.Const String) Data.Identity.Identity String String
          columnForm column =
            Formlet.form \_ ->
              { render: Data.Const.Const
              , validate:
                  \row ->
                    let
                      value = column <> row
                    in
                      if Data.String.length value > 10 then Right value else Left value
              }

          form :: forall config. Formlet.Form config _ Data.Identity.Identity (Formlet.Ocelot.KeyedArray.KeyedArray String) (Array (Array String))
          form =
            Formlet.Ocelot.Table.table
              $ traverse (\column -> Formlet.Ocelot.Table.column column (columnForm column))
              $ columns

          allValues :: Array (Array String)
          allValues = map (\row -> map (_ <> row) columns) rows

          expected :: Either Formlet.Errors (Array (Array String))
          expected = case Data.Array.find (\value -> Data.String.length value <= 10) (join allValues) of
            Nothing -> Right allValues
            Just error -> Left [ error ]

          actual :: Either Formlet.Errors (Array (Array String))
          actual = Formlet.validate form unit (Formlet.Ocelot.KeyedArray.fromArray rows)
        in
          expected === actual

identityForm ::
  forall config m value.
  Applicative m =>
  value ->
  Formlet.Form config Data.Identity.Identity m value value
identityForm newValue = Formlet.form_ \_ _ -> Data.Identity.Identity (pure \_ -> newValue)

prefixForm ::
  forall config m value.
  Semigroup value =>
  value ->
  Formlet.Form config (Data.Const.Const value) m value value
prefixForm prefix = Formlet.form_ \_ -> Data.Const.Const <<< append prefix

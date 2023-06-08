module Test.Form2.Ocelot.Table
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Data.Const as Data.Const
import Data.Foldable as Data.Foldable
import Data.Identity as Data.Identity
import Data.String as Data.String
import Form2 as Form2
import Form2.Ocelot.KeyedArray as Form2.Ocelot.KeyedArray
import Form2.Ocelot.Table as Form2.Ocelot.Table
import Form2.Render as Form2.Render
import Form2.Render.List as Form2.Render.List
import Test.QuickCheck ((===))
import Test.QuickCheck.Gen as Test.QuickCheck.Gen
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Form2.Ocelot.Table" do
    Test.Unit.test "`table` should render all column headers" do
      Test.Unit.QuickCheck.quickCheck \columns ->
        let
          rendered :: Form2.Ocelot.Table.Render Data.Identity.Identity (Form2.Ocelot.KeyedArray.KeyedArray Unit -> Form2.Ocelot.KeyedArray.KeyedArray Unit)
          rendered =
            Form2.Render.match { table: map (un Data.Identity.Identity) }
              $ Form2.render
                  ( Form2.Ocelot.Table.table
                      $ traverse (\column -> Form2.Ocelot.Table.column column (pure unit))
                      $ columns
                  )
                  unit
                  (Form2.Ocelot.KeyedArray.fromArray [])

          expected :: Array String
          expected = columns

          actual :: Array String
          actual = (un Form2.Ocelot.Table.Render rendered).header
        in
          expected === actual
    Test.Unit.test "`table` should render all rows and all columns" do
      Test.Unit.QuickCheck.quickCheck \columns rows ->
        let
          rendered :: Form2.Ocelot.Table.Render (Data.Const.Const String) (Form2.Ocelot.KeyedArray.KeyedArray String -> Form2.Ocelot.KeyedArray.KeyedArray String)
          rendered =
            Form2.Render.match { table: map (un Data.Identity.Identity) }
              $ Form2.render
                  ( Form2.Ocelot.Table.table
                      $ traverse (\column -> Form2.Ocelot.Table.column column (prefixForm column))
                      $ columns
                  )
                  unit
                  (Form2.Ocelot.KeyedArray.fromArray rows)

          expected :: Array (Array String)
          expected = map (\row -> map (_ <> row) columns) rows

          actual :: Array (Array String)
          actual =
            map (map (un Data.Const.Const) <<< Form2.Render.List.toArray <<< _.columns <<< un Form2.Ocelot.Table.RenderRow)
              $ Form2.Render.List.toArray
              $ (un Form2.Ocelot.Table.Render rendered).rows
        in
          expected === actual
    Test.Unit.test "`table` should correctly route updates to the form state" do
      Test.Unit.QuickCheck.quickCheck \(columns :: NonEmptyArray String) rows newValue -> do
        index <- Test.QuickCheck.Gen.chooseInt 0 (Data.Foldable.length rows)
        let
          rendered :: Form2.Ocelot.Table.Render Data.Identity.Identity (Form2.Ocelot.KeyedArray.KeyedArray String -> Form2.Ocelot.KeyedArray.KeyedArray String)
          rendered =
            Form2.Render.match { table: map (un Data.Identity.Identity) }
              $ Form2.render
                  ( Form2.Ocelot.Table.table
                      $ traverse (\column -> Form2.Ocelot.Table.column column (identityForm newValue))
                      $ columns
                  )
                  unit
                  rows

          array :: Array String
          array = Form2.Ocelot.KeyedArray.toArray rows

          expected :: Array String
          expected = fromMaybe array (Data.Array.modifyAt index (const newValue) array)

          actual :: Array String
          actual =
            let
              Form2.Render.List.List renderedRows = (un Form2.Ocelot.Table.Render rendered).rows
            in
              fromMaybe array do
                { render: row } <- Data.Array.index renderedRows index
                let
                  Form2.Ocelot.Table.RenderRow { columns: Form2.Render.List.List renderedColumns } = row
                -- It doesn't matter which column we pick here as all columns
                -- are editing the same value.
                { render } <- Data.Array.head renderedColumns
                pure $ Form2.Ocelot.KeyedArray.toArray (un Data.Identity.Identity render rows)
        pure $ expected === actual
    Test.Unit.test "`table` should produce only the first validation error of all cells" do
      Test.Unit.QuickCheck.quickCheck \columns rows ->
        let
          columnForm :: forall config. String -> Form2.Form config (Data.Const.Const String) Data.Identity.Identity String String
          columnForm column =
            Form2.form \_ ->
              { render: Data.Const.Const
              , validate:
                  \row ->
                    let
                      value = column <> row
                    in
                      if Data.String.length value > 10 then Right value else Left value
              }

          form :: forall config. Form2.Form config _ Data.Identity.Identity (Form2.Ocelot.KeyedArray.KeyedArray String) (Array (Array String))
          form =
            Form2.Ocelot.Table.table
              $ traverse (\column -> Form2.Ocelot.Table.column column (columnForm column))
              $ columns

          allValues :: Array (Array String)
          allValues = map (\row -> map (_ <> row) columns) rows

          expected :: Either Form2.Errors (Array (Array String))
          expected = case Data.Array.find (\value -> Data.String.length value <= 10) (join allValues) of
            Nothing -> Right allValues
            Just error -> Left [ error ]

          actual :: Either Form2.Errors (Array (Array String))
          actual = Form2.validate form unit (Form2.Ocelot.KeyedArray.fromArray rows)
        in
          expected === actual

identityForm ::
  forall config m value.
  Applicative m =>
  value ->
  Form2.Form config Data.Identity.Identity m value value
identityForm newValue = Form2.form_ \_ _ -> Data.Identity.Identity (pure \_ -> newValue)

prefixForm ::
  forall config m value.
  Semigroup value =>
  value ->
  Form2.Form config (Data.Const.Const value) m value value
prefixForm prefix = Form2.form_ \_ -> Data.Const.Const <<< append prefix

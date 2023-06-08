module Test.Form2.Ocelot.Sequence
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Data.Const as Data.Const
import Data.Foldable as Data.Foldable
import Data.Identity as Data.Identity
import Form2 as Form2
import Form2.Ocelot.KeyedArray as Form2.Ocelot.KeyedArray
import Form2.Ocelot.Sequence as Form2.Ocelot.Sequence
import Form2.Render as Form2.Render
import Form2.Render.List as Form2.Render.List
import Option as Option
import Test.QuickCheck ((===))
import Test.QuickCheck.Arbitrary as Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen as Test.QuickCheck.Gen
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Form2.Ocelot.Sequence" do
    Test.Unit.test "`sequence` should render all sections" do
      Test.Unit.QuickCheck.quickCheck \values ->
        let
          rendered :: Form2.Ocelot.Sequence.Render (Data.Const.Const String) (Form2.Ocelot.Sequence.Sequence String -> Form2.Ocelot.Sequence.Sequence String)
          rendered =
            Form2.Render.match { sequence: map (un Data.Identity.Identity) }
              $ Form2.render (constSequenceForm { defaultValue: Just $ pure "" }) { readonly: false }
              $ Form2.Ocelot.Sequence.fromArray values

          expected :: forall action. Array { key :: Form2.Render.List.Key, render :: Data.Const.Const String action }
          expected =
            values
              # Data.Array.mapWithIndex \index value ->
                  { key: show index
                  , render: Data.Const.Const value
                  }

          actual :: Array { key :: Form2.Render.List.Key, render :: Data.Const.Const String Unit }
          actual =
            un Form2.Render.List.List
              $ Form2.Render.List.hoist (\(Form2.Ocelot.Sequence.SectionRender s) -> s.render)
              $ void
              $ _.sections
              $ un Form2.Ocelot.Sequence.Render
              $ rendered
        in
          expected === actual
    Test.Unit.test "`sequence`'s `onAddSection` action should add a new section to the end using `defaultValue`" do
      Test.Unit.QuickCheck.quickCheck \defaultValue sequence ->
        let
          expected :: Array String
          expected = Data.Array.snoc (Form2.Ocelot.KeyedArray.toArray sequence) defaultValue

          actual :: Array String
          actual =
            Form2.Ocelot.KeyedArray.toArray
              $ performAction (constSequenceForm { defaultValue: Just $ pure defaultValue }) { readonly: false } AddSection
              $ sequence
        in
          expected === actual
    Test.Unit.test "`sequence`'s `onAddSection` action should be muted if `defaultValue` is set to `Nothing`" do
      Test.Unit.QuickCheck.quickCheck \sequence ->
        let
          expected :: Array String
          expected = Form2.Ocelot.KeyedArray.toArray sequence

          actual :: Array String
          actual =
            Form2.Ocelot.KeyedArray.toArray
              $ performAction (constSequenceForm { defaultValue: Nothing }) { readonly: false } AddSection
              $ sequence
        in
          expected === actual
    Test.Unit.test "`sequence`'s `onMove` action should move the specified section to a new position" do
      Test.Unit.QuickCheck.quickCheck \(defaultValue :: String) sequence -> do
        let
          length = Data.Foldable.length sequence
        from <- Test.QuickCheck.Gen.chooseInt 0 length
        to <- Test.QuickCheck.Gen.chooseInt 0 length
        let
          array :: Array String
          array = Form2.Ocelot.KeyedArray.toArray sequence

          expected :: Array String
          expected =
            fromMaybe array do
              a <- Data.Array.index array from
              Data.Array.insertAt to a =<< Data.Array.deleteAt from array

          actual :: Array String
          actual =
            Form2.Ocelot.KeyedArray.toArray
              $ performAction (constSequenceForm { defaultValue: Just $ pure defaultValue }) { readonly: false } (Move from to)
              $ sequence
        pure $ expected === actual
    Test.Unit.test "`sequence`'s `onRemove` action should delete the specified section" do
      Test.Unit.QuickCheck.quickCheck \(defaultValue :: String) sequence -> do
        index <- Test.QuickCheck.Gen.chooseInt 0 (Data.Foldable.length sequence)
        let
          array :: Array String
          array = Form2.Ocelot.KeyedArray.toArray sequence

          expected :: Array String
          expected = fromMaybe array (Data.Array.deleteAt index array)

          actual :: Array String
          actual =
            Form2.Ocelot.KeyedArray.toArray
              $ performAction (constSequenceForm { defaultValue: Just $ pure defaultValue }) { readonly: false } (Remove index)
              $ sequence
        pure $ expected === actual
    Test.Unit.test "`sequence`'s `onRemove` action should be muted if `removable` is set to `false`" do
      Test.Unit.QuickCheck.quickCheck \(defaultValue :: String) sequence -> do
        index <- Test.QuickCheck.Gen.chooseInt 0 (Data.Foldable.length sequence)
        let
          expected :: Array String
          expected = Form2.Ocelot.KeyedArray.toArray sequence

          actual :: Array String
          actual =
            Form2.Ocelot.KeyedArray.toArray
              $ performAction (constSequenceForm { defaultValue: Just $ pure defaultValue, removable: false }) { readonly: false } (Remove index)
              $ sequence
        pure $ expected === actual
    Test.Unit.test "`sequence` should not allow any updates if `readonly = true`" do
      Test.Unit.QuickCheck.quickCheck \action defaultValue readonly sequence ->
        let
          expected :: Form2.Ocelot.Sequence.Sequence String
          expected = case action of
            AddSection -> if readonly then sequence else Form2.Ocelot.KeyedArray.snoc defaultValue sequence
            Remove index -> if readonly then sequence else Form2.Ocelot.KeyedArray.delete index sequence
            Move from to -> if readonly then sequence else Form2.Ocelot.KeyedArray.move from to sequence

          actual :: Form2.Ocelot.Sequence.Sequence String
          actual = performAction (constSequenceForm { defaultValue: Just $ pure defaultValue }) { readonly } action sequence
        in
          expected === actual
    Test.Unit.test "Updates to a sequence section should be routed to the correct position in the form state array" do
      Test.Unit.QuickCheck.quickCheck \sequence value' -> do
        index <- Test.QuickCheck.Gen.chooseInt 0 (Data.Foldable.length sequence)
        let
          rendered :: Form2.Ocelot.Sequence.Render Data.Identity.Identity (Form2.Ocelot.Sequence.Sequence String -> Form2.Ocelot.Sequence.Sequence String)
          rendered =
            Form2.Render.match { sequence: map (un Data.Identity.Identity) }
              $ Form2.render
                  ( Form2.Ocelot.Sequence.sequence
                      { label: "", defaultValue: Just $ pure "" }
                      (Form2.form_ \_ _ -> Data.Identity.Identity (pure \_ -> value'))
                  )
                  { readonly: false }
                  sequence

          array :: Array String
          array = Form2.Ocelot.KeyedArray.toArray sequence

          expected :: Array String
          expected = fromMaybe array (Data.Array.modifyAt index (const value') array)

          actual :: Array String
          actual =
            let
              Form2.Render.List.List sections = (un Form2.Ocelot.Sequence.Render rendered).sections
            in
              fromMaybe array ado
                { render: Form2.Ocelot.Sequence.SectionRender { render } } <- Data.Array.index sections index
                in Form2.Ocelot.KeyedArray.toArray (un Data.Identity.Identity render sequence)
        pure $ expected === actual

-----------
-- Internal
-----------
data Action
  = AddSection
  | Remove Int
  | Move Int Int

derive instance Generic Action _

instance Test.QuickCheck.Arbitrary.Arbitrary Action where
  arbitrary = Test.QuickCheck.Arbitrary.genericArbitrary

type SequenceForm config render m value result =
  Form2.Form
    { readonly :: Boolean | config }
    (Form2.Render.Render () (sequence :: Form2.Ocelot.Sequence.Render render))
    m
    (Form2.Ocelot.Sequence.Sequence value)
    (Array result)

constSequenceForm ::
  forall config m polyParams value.
  Applicative m =>
  Option.FromRecord polyParams Form2.Ocelot.Sequence.ParamsRequired (Form2.Ocelot.Sequence.ParamsOptional m value) =>
  Option.ToRecord Form2.Ocelot.Sequence.ParamsRequired (Form2.Ocelot.Sequence.ParamsOptional m value) (Form2.Ocelot.Sequence.Params m value) =>
  Record polyParams ->
  SequenceForm config (Data.Const.Const value) m value value
constSequenceForm polyParams =
  Form2.Ocelot.Sequence.sequence polyParams
    $ Form2.form_ \_ -> Data.Const.Const

performAction ::
  forall config render value result.
  Functor render =>
  SequenceForm config render Data.Identity.Identity value result ->
  { readonly :: Boolean | config } ->
  Action ->
  Form2.Ocelot.Sequence.Sequence value ->
  Form2.Ocelot.Sequence.Sequence value
performAction form config action value =
  let
    rendered :: Form2.Ocelot.Sequence.Render render (Form2.Ocelot.Sequence.Sequence value -> Form2.Ocelot.Sequence.Sequence value)
    rendered =
      Form2.Render.match { sequence: map (un Data.Identity.Identity) }
        $ Form2.render form config
        $ value

    sections :: Array (Form2.Ocelot.Sequence.SectionRender render (Form2.Ocelot.Sequence.Sequence value -> Form2.Ocelot.Sequence.Sequence value))
    sections =
      map _.render
        $ un Form2.Render.List.List
        $ _.sections
        $ un Form2.Ocelot.Sequence.Render
        $ rendered
  in
    case action of
      AddSection -> (un Form2.Ocelot.Sequence.Render rendered).onAddSection value
      Remove index -> case Data.Array.index sections index of
        Nothing -> value
        Just (Form2.Ocelot.Sequence.SectionRender s) -> s.onRemove value
      Move from to -> case Data.Array.index sections from of
        Nothing -> value
        Just (Form2.Ocelot.Sequence.SectionRender s) -> s.onMove to value

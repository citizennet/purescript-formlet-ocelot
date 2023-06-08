module Test.Form2.Ocelot.Modal.Halogen
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Const as Data.Const
import Debug as Debug
import Effect.Aff as Effect.Aff
import Form2 as Form2
import Form2.Ocelot.Modal.Halogen as Form2.Ocelot.Modal.Halogen
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.Subscription as Halogen.Subscription
import Halogen.Test.Driver as Halogen.Test.Driver
import Halogen.Test.Subscription as Halogen.Test.Subscription
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert
import Test.Utils as Test.Utils

suite :: Test.Unit.TestSuite
suite =
  -- In this test suite we're not testing queries that just defer the call to
  -- `Form2.Managed.Halogen`, as those are already tested in
  -- `Test.Form2.Managed.Halogen`.
  Test.Unit.suite "Form2.Ocelot.Modal.Halogen" do
    Test.Unit.test "the `Open` query should open the modal" do
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Form2.Ocelot.Modal.Halogen.component constForm renderConstText {})
          { disabled: false
          , mainAction: "Submit"
          , title: "Test Modal"
          }
      _ <- Effect.Aff.forkAff $ io.query (Form2.Ocelot.Modal.Halogen.Open unit "" identity)
      testIsOpen io true
    Test.Unit.test "the `Open` query should set the initial value" do
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Form2.Ocelot.Modal.Halogen.component constForm renderConstText {})
          { disabled: false
          , mainAction: "Submit"
          , title: "Test Modal"
          }
      _ <- Effect.Aff.forkAff $ io.query (Form2.Ocelot.Modal.Halogen.Open unit "initialValue" identity)
      actual <- io.query (Form2.Ocelot.Modal.Halogen.GetValue identity)
      Test.Utils.equal (Just "initialValue") actual
    Test.Unit.test "the `Open` query should block and return the validated result" do
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Form2.Ocelot.Modal.Halogen.component (map (_ <> "Result") constForm) renderConstText {})
          { disabled: false
          , mainAction: "Submit"
          , title: "Test Modal"
          }
      _ <- Effect.Aff.forkAff do
        Effect.Aff.delay (Effect.Aff.Milliseconds 100.0)
        io.query (Form2.Ocelot.Modal.Halogen.Submit identity)
      actual <- io.query (Form2.Ocelot.Modal.Halogen.Open unit "initialValue" identity)
      Test.Utils.equal (Just "initialValueResult") actual
    Test.Unit.test "the `Close` query should close the modal" do
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Form2.Ocelot.Modal.Halogen.component constForm renderConstText {})
          { disabled: false
          , mainAction: "Submit"
          , title: "Test Modal"
          }
      Test.Unit.Assert.expectFailure "Modal closed" do
        _ <- Effect.Aff.forkAff do
          Effect.Aff.delay (Effect.Aff.Milliseconds 100.0)
          testIsOpen io true
          io.query (Form2.Ocelot.Modal.Halogen.Close unit)
        void $ io.query (Form2.Ocelot.Modal.Halogen.Open unit "initialValue" identity)
      testIsOpen io false
    Test.Unit.test "the form should not be rendered before opening the modal" do
      { emitter, listener } <- liftEffect Halogen.Subscription.create
      let
        form ::
          forall config slots.
          Form2.Form config (Halogen.Test.Subscription.HTML Aff (Aff (String -> String)) slots) Aff String String
        form = Form2.form_ \_ _ -> Halogen.Test.Subscription.subscribe emitter Halogen.raise
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Form2.Ocelot.Modal.Halogen.component form (\_ -> identity) {})
          { disabled: false
          , mainAction: "Submit"
          , title: "Test Modal"
          }
      testValue io Nothing
      liftEffect $ Halogen.Subscription.notify listener (pure (_ <> "1"))
      testValue io Nothing
    Test.Unit.test "opening the modal should render the form" do
      { emitter, listener } <- liftEffect Halogen.Subscription.create
      let
        form ::
          forall config slots.
          Form2.Form config (Halogen.Test.Subscription.HTML Aff (Aff (String -> String)) slots) Aff String String
        form = Form2.form \_ ->
          { render: \_ -> Halogen.Test.Subscription.subscribe emitter Halogen.raise
          , validate: \value -> if value == "initialValue" then Left "invalid" else Right "result"
          }
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Form2.Ocelot.Modal.Halogen.component form (\_ -> identity) {})
          { disabled: false
          , mainAction: "Submit"
          , title: "Test Modal"
          }
      testIsOpen io false
      testValue io Nothing
      fiber <-
        Effect.Aff.forkAff
          $ io.query
          $ Form2.Ocelot.Modal.Halogen.Open unit "initialValue" identity
      -- We need this small delay so the forked `Open` query can be fully
      -- evaluated and the form actually rendered before we try sending updates
      Effect.Aff.delay (Effect.Aff.Milliseconds 0.0)
      testIsOpen io true
      liftEffect $ Halogen.Subscription.notify listener (pure (_ <> "1"))
      testValue io (Just "initialValue1")
      void $ io.query (Form2.Ocelot.Modal.Halogen.Submit identity)
      actual <- Effect.Aff.joinFiber fiber
      Test.Utils.equal (Just "result") actual
      testIsOpen io false
    Test.Unit.test "submitting the form should close the modal" do
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Form2.Ocelot.Modal.Halogen.component constForm renderConstText {})
          { disabled: false
          , mainAction: "Submit"
          , title: "Test Modal"
          }
      _ <- Effect.Aff.forkAff do
        Effect.Aff.delay (Effect.Aff.Milliseconds 100.0)
        testIsOpen io true
        io.query (Form2.Ocelot.Modal.Halogen.Submit identity)
      _ <- io.query (Form2.Ocelot.Modal.Halogen.Open unit "initialValue" identity)
      testIsOpen io false
    Test.Unit.test "submitting shouldn't be possible if `disabled` is `true`" do
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Form2.Ocelot.Modal.Halogen.component constForm renderConstText {})
          { disabled: true
          , mainAction: "Submit"
          , title: "Test Modal"
          }
      Test.Unit.Assert.expectFailure "Modal closed" do
        _ <- Effect.Aff.forkAff do
          Effect.Aff.delay (Effect.Aff.Milliseconds 100.0)
          testIsOpen io true
          actual <- io.query (Form2.Ocelot.Modal.Halogen.Submit identity)
          Test.Utils.equal Nothing actual
          testIsOpen io true
          io.query (Form2.Ocelot.Modal.Halogen.Close unit)
        void $ io.query (Form2.Ocelot.Modal.Halogen.Open unit "initialValue" identity)
      testIsOpen io false

-----------
-- Internal
-----------

constForm ::
  forall config m value.
  Form2.Form config (Data.Const.Const value) m value value
constForm = Form2.form_ \_ -> Data.Const.Const

renderConstText ::
  forall a config i p.
  config ->
  Data.Const.Const String a ->
  Halogen.HTML.HTML i p
renderConstText _ = Halogen.HTML.text <<< un Data.Const.Const

-- | Utility function for testing whether a form modal is open or not.
testIsOpen ::
  forall config output result value.
  Halogen.HalogenIO (Form2.Ocelot.Modal.Halogen.Query config value result) output Aff ->
  Boolean ->
  Aff Unit
testIsOpen io expected = do
  actual <- io.query (Form2.Ocelot.Modal.Halogen.IsOpen identity)
  Test.Utils.equal (Just expected) actual

-- | Utility function for testing the internal form state of a
-- | `Form2.Ocelot.Modal.Halogen` component.
testValue ::
  forall config output result value.
  Eq value =>
  Debug.Debug value =>
  Halogen.HalogenIO (Form2.Ocelot.Modal.Halogen.Query config value result) output Aff ->
  Maybe value ->
  Aff Unit
testValue io expected = do
  actual <- io.query (Form2.Ocelot.Modal.Halogen.GetValue identity)
  Test.Utils.equal expected actual

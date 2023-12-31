module Test.Formlet.Ocelot.Modal.Halogen
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Const as Data.Const
import Effect.Aff as Effect.Aff
import Formlet as Formlet
import Formlet.Ocelot.Modal.Halogen as Formlet.Ocelot.Modal.Halogen
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.Subscription as Halogen.Subscription
import Halogen.Test.Driver as Halogen.Test.Driver
import Halogen.Test.Subscription as Halogen.Test.Subscription
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert

suite :: Test.Unit.TestSuite
suite =
  -- In this test suite we're not testing queries that just defer the call to
  -- `Formlet.Managed.Halogen`, as those are already tested in
  -- `Test.Formlet.Managed.Halogen`.
  Test.Unit.suite "Formlet.Ocelot.Modal.Halogen" do
    Test.Unit.test "the `Open` query should open the modal" do
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Formlet.Ocelot.Modal.Halogen.component constForm renderConstText {})
          { disabled: false
          , mainAction: "Submit"
          , title: "Test Modal"
          }
      _ <- Effect.Aff.forkAff $ io.query (Formlet.Ocelot.Modal.Halogen.Open unit "" identity)
      testIsOpen io true
    Test.Unit.test "the `Open` query should set the initial value" do
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Formlet.Ocelot.Modal.Halogen.component constForm renderConstText {})
          { disabled: false
          , mainAction: "Submit"
          , title: "Test Modal"
          }
      _ <- Effect.Aff.forkAff $ io.query (Formlet.Ocelot.Modal.Halogen.Open unit "initialValue" identity)
      actual <- io.query (Formlet.Ocelot.Modal.Halogen.GetValue identity)
      Test.Unit.Assert.equal (Just "initialValue") actual
    Test.Unit.test "the `Open` query should block and return the validated result" do
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Formlet.Ocelot.Modal.Halogen.component (map (_ <> "Result") constForm) renderConstText {})
          { disabled: false
          , mainAction: "Submit"
          , title: "Test Modal"
          }
      _ <- Effect.Aff.forkAff do
        Effect.Aff.delay (Effect.Aff.Milliseconds 100.0)
        io.query (Formlet.Ocelot.Modal.Halogen.Submit identity)
      actual <- io.query (Formlet.Ocelot.Modal.Halogen.Open unit "initialValue" identity)
      Test.Unit.Assert.equal (Just "initialValueResult") actual
    Test.Unit.test "the `Close` query should close the modal" do
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Formlet.Ocelot.Modal.Halogen.component constForm renderConstText {})
          { disabled: false
          , mainAction: "Submit"
          , title: "Test Modal"
          }
      Test.Unit.Assert.expectFailure "Modal closed" do
        _ <- Effect.Aff.forkAff do
          Effect.Aff.delay (Effect.Aff.Milliseconds 100.0)
          testIsOpen io true
          io.query (Formlet.Ocelot.Modal.Halogen.Close unit)
        void $ io.query (Formlet.Ocelot.Modal.Halogen.Open unit "initialValue" identity)
      testIsOpen io false
    Test.Unit.test "the form should not be rendered before opening the modal" do
      { emitter, listener } <- liftEffect Halogen.Subscription.create
      let
        form ::
          forall config slots.
          Formlet.Form config (Halogen.Test.Subscription.HTML Aff (Aff (String -> String)) slots) Aff String String
        form = Formlet.form_ \_ _ -> Halogen.Test.Subscription.subscribe emitter Halogen.raise
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Formlet.Ocelot.Modal.Halogen.component form (\_ -> identity) {})
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
          Formlet.Form config (Halogen.Test.Subscription.HTML Aff (Aff (String -> String)) slots) Aff String String
        form = Formlet.form \_ ->
          { render: \_ -> Halogen.Test.Subscription.subscribe emitter Halogen.raise
          , validate: \value -> if value == "initialValue" then Left "invalid" else Right "result"
          }
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Formlet.Ocelot.Modal.Halogen.component form (\_ -> identity) {})
          { disabled: false
          , mainAction: "Submit"
          , title: "Test Modal"
          }
      testIsOpen io false
      testValue io Nothing
      fiber <-
        Effect.Aff.forkAff
          $ io.query
          $ Formlet.Ocelot.Modal.Halogen.Open unit "initialValue" identity
      -- We need this small delay so the forked `Open` query can be fully
      -- evaluated and the form actually rendered before we try sending updates
      Effect.Aff.delay (Effect.Aff.Milliseconds 0.0)
      testIsOpen io true
      liftEffect $ Halogen.Subscription.notify listener (pure (_ <> "1"))
      testValue io (Just "initialValue1")
      void $ io.query (Formlet.Ocelot.Modal.Halogen.Submit identity)
      actual <- Effect.Aff.joinFiber fiber
      Test.Unit.Assert.equal (Just "result") actual
      testIsOpen io false
    Test.Unit.test "submitting the form should close the modal" do
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Formlet.Ocelot.Modal.Halogen.component constForm renderConstText {})
          { disabled: false
          , mainAction: "Submit"
          , title: "Test Modal"
          }
      _ <- Effect.Aff.forkAff do
        Effect.Aff.delay (Effect.Aff.Milliseconds 100.0)
        testIsOpen io true
        io.query (Formlet.Ocelot.Modal.Halogen.Submit identity)
      _ <- io.query (Formlet.Ocelot.Modal.Halogen.Open unit "initialValue" identity)
      testIsOpen io false
    Test.Unit.test "submitting shouldn't be possible if `disabled` is `true`" do
      io <-
        Halogen.Test.Driver.runUI
          { duplicateSlot: mempty }
          (Formlet.Ocelot.Modal.Halogen.component constForm renderConstText {})
          { disabled: true
          , mainAction: "Submit"
          , title: "Test Modal"
          }
      Test.Unit.Assert.expectFailure "Modal closed" do
        _ <- Effect.Aff.forkAff do
          Effect.Aff.delay (Effect.Aff.Milliseconds 100.0)
          testIsOpen io true
          actual <- io.query (Formlet.Ocelot.Modal.Halogen.Submit identity)
          Test.Unit.Assert.equal Nothing actual
          testIsOpen io true
          io.query (Formlet.Ocelot.Modal.Halogen.Close unit)
        void $ io.query (Formlet.Ocelot.Modal.Halogen.Open unit "initialValue" identity)
      testIsOpen io false

-----------
-- Internal
-----------

constForm ::
  forall config m value.
  Formlet.Form config (Data.Const.Const value) m value value
constForm = Formlet.form_ \_ -> Data.Const.Const

renderConstText ::
  forall a config i p.
  config ->
  Data.Const.Const String a ->
  Halogen.HTML.HTML i p
renderConstText _ = Halogen.HTML.text <<< un Data.Const.Const

-- | Utility function for testing whether a form modal is open or not.
testIsOpen ::
  forall config output result value.
  Halogen.HalogenIO (Formlet.Ocelot.Modal.Halogen.Query config value result) output Aff ->
  Boolean ->
  Aff Unit
testIsOpen io expected = do
  actual <- io.query (Formlet.Ocelot.Modal.Halogen.IsOpen identity)
  Test.Unit.Assert.equal (Just expected) actual

-- | Utility function for testing the internal form state of a
-- | `Formlet.Ocelot.Modal.Halogen` component.
testValue ::
  forall config output result value.
  Eq value =>
  Show value =>
  Halogen.HalogenIO (Formlet.Ocelot.Modal.Halogen.Query config value result) output Aff ->
  Maybe value ->
  Aff Unit
testValue io expected = do
  actual <- io.query (Formlet.Ocelot.Modal.Halogen.GetValue identity)
  Test.Unit.Assert.equal expected actual

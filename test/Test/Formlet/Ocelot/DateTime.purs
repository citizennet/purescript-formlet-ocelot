module Test.Formlet.Ocelot.DateTime
  ( suite
  ) where

import CitizenNet.Prelude

import Control.Monad.Gen.Common as Control.Monad.Gen.Common
import Data.DateTime.Gen as Data.DateTime.Gen
import Data.Identity as Data.Identity
import Formlet as Formlet
import Formlet.Ocelot.DateTime as Formlet.Ocelot.DateTime
import Formlet.Render as Formlet.Render
import Test.QuickCheck ((===))
import Test.QuickCheck.Gen as Test.QuickCheck.Gen
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck
import TimeZone as TimeZone

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Formlet.Ocelot.DateTime" do
    Test.Unit.test "`dateTime` should not change its value if `readonly = true`" do
      Test.Unit.QuickCheck.quickCheck \readonly -> do
        value <- Control.Monad.Gen.Common.genMaybe Data.DateTime.Gen.genDateTime :: Test.QuickCheck.Gen.Gen _
        value' <- Control.Monad.Gen.Common.genMaybe Data.DateTime.Gen.genDateTime
        let
          rendered :: Formlet.Ocelot.DateTime.Render (Maybe DateTime -> Maybe DateTime)
          rendered =
            Formlet.Render.match { dateTime: map (un Data.Identity.Identity) }
              $ Formlet.render (Formlet.Ocelot.DateTime.dateTime {})
                  { readonly
                  , timezone: TimeZone.utc
                  }
                  value

          expected :: Maybe DateTime
          expected = if readonly then value else value'
        pure $ expected === (un Formlet.Ocelot.DateTime.Render rendered).onChange value' value

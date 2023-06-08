module Test.Form2.Ocelot.DateTime
  ( suite
  ) where

import CitizenNet.Prelude

import Control.Monad.Gen.Common as Control.Monad.Gen.Common
import Data.DateTime.Gen as Data.DateTime.Gen
import Data.Identity as Data.Identity
import Form2 as Form2
import Form2.Ocelot.DateTime as Form2.Ocelot.DateTime
import Form2.Render as Form2.Render
import Test.QuickCheck ((===))
import Test.QuickCheck.Gen as Test.QuickCheck.Gen
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck
import TimeZone as TimeZone

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Form2.Ocelot.DateTime" do
    Test.Unit.test "`dateTime` should not change its value if `readonly = true`" do
      Test.Unit.QuickCheck.quickCheck \readonly -> do
        value <- Control.Monad.Gen.Common.genMaybe Data.DateTime.Gen.genDateTime :: Test.QuickCheck.Gen.Gen _
        value' <- Control.Monad.Gen.Common.genMaybe Data.DateTime.Gen.genDateTime
        let
          rendered :: Form2.Ocelot.DateTime.Render (Maybe DateTime -> Maybe DateTime)
          rendered =
            Form2.Render.match { dateTime: map (un Data.Identity.Identity) }
              $ Form2.render (Form2.Ocelot.DateTime.dateTime { placeholder: "" })
                  { readonly
                  , timezone: TimeZone.utc
                  }
                  value

          expected :: Maybe DateTime
          expected = if readonly then value else value'
        pure $ expected === (un Form2.Ocelot.DateTime.Render rendered).onChange value' value

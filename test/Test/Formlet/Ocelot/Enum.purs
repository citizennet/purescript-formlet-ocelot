module Test.Formlet.Ocelot.Enum
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Bounded.Generic as Data.Bounded.Generic
import Data.Enum.Generic as Data.Enum.Generic
import Data.Show.Generic as Data.Show.Generic
import Debug as Debug
import Formlet.Ocelot.Enum as Formlet.Ocelot.Enum
import Test.Unit as Test.Unit
import Test.Utils as Test.Utils

data TestEnum
  = A
  | B
  | C
  | D
  | E

derive instance Generic TestEnum _
derive instance Eq TestEnum
derive instance Ord TestEnum

instance Debug.Debug TestEnum where
  debug = Debug.genericDebug

instance Show TestEnum where
  show = Data.Show.Generic.genericShow

instance Enum TestEnum where
  succ = Data.Enum.Generic.genericSucc
  pred = Data.Enum.Generic.genericPred

instance Bounded TestEnum where
  top = Data.Bounded.Generic.genericTop
  bottom = Data.Bounded.Generic.genericBottom

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Formlet.Ocelot.Enum" do
    Test.Unit.test "`enumOptions` should produce all values of a `Bounded` `Enum`" do
      Test.Utils.equal [ A, B, C, D, E ] Formlet.Ocelot.Enum.enumOptions
    Test.Unit.test "`genericEnumOptions` should produce all unary constructors of a `Generic` data type" do
      Test.Utils.equal [ A, B, C, D, E ] Formlet.Ocelot.Enum.genericEnumOptions

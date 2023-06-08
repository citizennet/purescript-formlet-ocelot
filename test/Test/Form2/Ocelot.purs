module Test.Form2.Ocelot
  ( main
  ) where

import CitizenNet.Prelude

import Test.Form2.Ocelot.Checkbox as Test.Form2.Ocelot.Checkbox
import Test.Form2.Ocelot.Currency as Test.Form2.Ocelot.Currency
import Test.Form2.Ocelot.DateTime as Test.Form2.Ocelot.DateTime
import Test.Form2.Ocelot.Dropdown as Test.Form2.Ocelot.Dropdown
import Test.Form2.Ocelot.Enum as Test.Form2.Ocelot.Enum
import Test.Form2.Ocelot.File as Test.Form2.Ocelot.File
import Test.Form2.Ocelot.Modal.Halogen as Test.Form2.Ocelot.Modal.Halogen
import Test.Form2.Ocelot.Radio as Test.Form2.Ocelot.Radio
import Test.Form2.Ocelot.Sequence as Test.Form2.Ocelot.Sequence
import Test.Form2.Ocelot.Table as Test.Form2.Ocelot.Table
import Test.Form2.Ocelot.Tags as Test.Form2.Ocelot.Tags
import Test.Form2.Ocelot.Text as Test.Form2.Ocelot.Text
import Test.Form2.Ocelot.Textarea as Test.Form2.Ocelot.Textarea
import Test.Form2.Ocelot.Toggle as Test.Form2.Ocelot.Toggle
import Test.Form2.Ocelot.Typeahead as Test.Form2.Ocelot.Typeahead
import Test.Form2.Ocelot.URL as Test.Form2.Ocelot.URL
import Test.Unit.Main as Test.Unit.Main

main :: Effect Unit
main =
  Test.Unit.Main.runTest do
    Test.Form2.Ocelot.Checkbox.suite
    Test.Form2.Ocelot.Currency.suite
    Test.Form2.Ocelot.DateTime.suite
    Test.Form2.Ocelot.Dropdown.suite
    Test.Form2.Ocelot.Enum.suite
    Test.Form2.Ocelot.File.suite
    Test.Form2.Ocelot.Modal.Halogen.suite
    Test.Form2.Ocelot.Radio.suite
    Test.Form2.Ocelot.Sequence.suite
    Test.Form2.Ocelot.Table.suite
    Test.Form2.Ocelot.Tags.suite
    Test.Form2.Ocelot.Text.suite
    Test.Form2.Ocelot.Textarea.suite
    Test.Form2.Ocelot.Toggle.suite
    Test.Form2.Ocelot.Typeahead.suite
    Test.Form2.Ocelot.URL.suite

module Test.Formlet.Ocelot
  ( main
  ) where

import CitizenNet.Prelude

import Test.Formlet.Ocelot.Checkbox as Test.Formlet.Ocelot.Checkbox
import Test.Formlet.Ocelot.Currency as Test.Formlet.Ocelot.Currency
import Test.Formlet.Ocelot.DateTime as Test.Formlet.Ocelot.DateTime
import Test.Formlet.Ocelot.Dropdown as Test.Formlet.Ocelot.Dropdown
import Test.Formlet.Ocelot.Enum as Test.Formlet.Ocelot.Enum
import Test.Formlet.Ocelot.File as Test.Formlet.Ocelot.File
import Test.Formlet.Ocelot.Modal.Halogen as Test.Formlet.Ocelot.Modal.Halogen
import Test.Formlet.Ocelot.Radio as Test.Formlet.Ocelot.Radio
import Test.Formlet.Ocelot.Sequence as Test.Formlet.Ocelot.Sequence
import Test.Formlet.Ocelot.Table as Test.Formlet.Ocelot.Table
import Test.Formlet.Ocelot.Tags as Test.Formlet.Ocelot.Tags
import Test.Formlet.Ocelot.Text as Test.Formlet.Ocelot.Text
import Test.Formlet.Ocelot.Textarea as Test.Formlet.Ocelot.Textarea
import Test.Formlet.Ocelot.Toggle as Test.Formlet.Ocelot.Toggle
import Test.Formlet.Ocelot.Typeahead as Test.Formlet.Ocelot.Typeahead
import Test.Formlet.Ocelot.URL as Test.Formlet.Ocelot.URL
import Test.Unit.Main as Test.Unit.Main

main :: Effect Unit
main =
  Test.Unit.Main.runTest do
    Test.Formlet.Ocelot.Checkbox.suite
    Test.Formlet.Ocelot.Currency.suite
    Test.Formlet.Ocelot.DateTime.suite
    Test.Formlet.Ocelot.Dropdown.suite
    Test.Formlet.Ocelot.Enum.suite
    Test.Formlet.Ocelot.File.suite
    Test.Formlet.Ocelot.Modal.Halogen.suite
    Test.Formlet.Ocelot.Radio.suite
    Test.Formlet.Ocelot.Sequence.suite
    Test.Formlet.Ocelot.Table.suite
    Test.Formlet.Ocelot.Tags.suite
    Test.Formlet.Ocelot.Text.suite
    Test.Formlet.Ocelot.Textarea.suite
    Test.Formlet.Ocelot.Toggle.suite
    Test.Formlet.Ocelot.Typeahead.suite
    Test.Formlet.Ocelot.URL.suite

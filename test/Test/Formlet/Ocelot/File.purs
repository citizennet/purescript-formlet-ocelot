module Test.Formlet.Ocelot.File
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Identity as Data.Identity
import Data.String.NonEmpty as Data.String.NonEmpty
import Data.These as Data.These
import Formlet as Formlet
import Formlet.Ocelot.File as Formlet.Ocelot.File
import Formlet.Render as Formlet.Render
import Test.Unit as Test.Unit
import Test.Utils as Test.Utils
import URI as URI
import URI.Host.RegName as URI.Host.RegName
import URI.Path.Segment as URI.Path.Segment
import URI.Scheme.Common as URI.Scheme.Common
import Unsafe.Coerce as Unsafe.Coerce
import Web.File.File as Web.File.File

type TestUploadedFile =
  { name :: String
  }

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Formlet.Ocelot.File" do
    Test.Unit.suite "readonly" do
      let
        file1 :: Maybe TestUploadedFile
        file1 = Just { name: "file1" }

        file2 :: Maybe TestUploadedFile
        file2 = Just { name: "file2" }

        render ::
          Maybe TestUploadedFile ->
          { readonly :: Boolean } ->
          Formlet.Ocelot.File.Render (Maybe TestUploadedFile -> Maybe TestUploadedFile)
        render fileToUpload { readonly } =
          map (un Data.Identity.Identity)
            $ Formlet.render (testFileForm fileToUpload) { readonly }
            $ file1
      Test.Unit.test "`file` should not change its value if `readonly = true`" do
        Formlet.Ocelot.File.withRender (render file2 { readonly: true }) \rendered -> do
          uploadedFile <- rendered.upload mempty mockFile
          Test.Utils.equal file1 (rendered.onChange (hush uploadedFile) file1)
      Test.Unit.test "`file` should change its value if `readonly = false`" do
        Formlet.Ocelot.File.withRender (render file2 { readonly: false }) \rendered -> do
          uploadedFile <- rendered.upload mempty mockFile
          Test.Utils.equal file2 (rendered.onChange (hush uploadedFile) file1)

-- The `File` API is only available in the browser environment, so we can't
-- create a valid one here. We're not really using this `File` in the tests
-- anyway, so we just make up a value with `unsafeCoerce` to satisfy the types.
mockFile :: Web.File.File.File
mockFile = Unsafe.Coerce.unsafeCoerce {}

testFileForm ::
  forall config.
  Maybe TestUploadedFile ->
  Formlet.Form
    { readonly :: Boolean
    | config
    }
    Formlet.Ocelot.File.Render
    Data.Identity.Identity
    (Maybe TestUploadedFile)
    (Maybe TestUploadedFile)
testFileForm fileToUpload =
  Formlet.mapRender (Formlet.Render.match { file: \render -> render })
    $ Formlet.Ocelot.File.file
        { download
        , getId: _.name
        , mediaTypes: []
        , upload
        }
  where
  download :: Formlet.Ocelot.File.URI
  download =
    URI.URI
      URI.Scheme.Common.http
      ( URI.HierarchicalPartAuth
          ( URI.Authority Nothing
              $ Just
              $ Data.These.This
              $ URI.NameAddress
              $ URI.Host.RegName.fromString
              $ Data.String.NonEmpty.nes
              $ symbol { "citizennet.com": _ }
          )
          (URI.Path (map URI.Path.Segment.segmentFromString [ "uploads", "" ]))
      )
      Nothing
      Nothing

  upload ::
    (Formlet.Ocelot.File.Progress -> Effect Unit) ->
    Web.File.File.File ->
    Aff (Either String TestUploadedFile)
  upload _ _ =
    pure
      $ Right
      $
        { name: maybe "" _.name fileToUpload
        }

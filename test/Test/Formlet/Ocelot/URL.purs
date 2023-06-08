module Test.Formlet.Ocelot.URL
  ( suite
  ) where

import CitizenNet.Prelude

import Control.Monad.Gen.Common as Control.Monad.Gen.Common
import Data.Either as Data.Either
import Data.Enum as Data.Enum
import Data.Identity as Data.Identity
import Data.String.CodeUnits as Data.String.CodeUnits
import Data.String.NonEmpty as Data.String.NonEmpty
import Formlet as Formlet
import Formlet.Ocelot.Text as Formlet.Ocelot.Text
import Formlet.Ocelot.URL as Formlet.Ocelot.URL
import Formlet.Render as Formlet.Render
import Test.QuickCheck ((===))
import Test.QuickCheck.Gen as Test.QuickCheck.Gen
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck
import Text.Parsing.Parser as Text.Parsing.Parser
import URI as URI
import URI.Host.Gen as URI.Host.Gen
import URI.HostPortPair as URI.HostPortPair
import URI.HostPortPair.Gen as URI.HostPortPair.Gen
import URI.Path.Segment as URI.Path.Segment
import URI.Port.Gen as URI.Port.Gen
import URI.Scheme as URI.Scheme
import URI.Scheme.Common as URI.Scheme.Common
import URI.URI as URI.URI

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Formlet.Ocelot.URL" do
    Test.Unit.test "`url` sets the `addonLeft` of the rendered `Formlet.Ocelot.Text.Render` if the `scheme` param is passed" do
      Test.Unit.QuickCheck.quickCheck \value -> ado
        scheme <- Control.Monad.Gen.Common.genMaybe genScheme
        let
          rendered :: Formlet.Ocelot.Text.Render (String -> String)
          rendered =
            Formlet.Render.match { text: map (un Data.Identity.Identity) }
              $ Formlet.render (Formlet.Ocelot.URL.url { scheme }) { readonly: false }
              $ value

          expected :: Maybe String
          expected = addSchemePrefix <$> scheme <@> ""
        in expected === (un Formlet.Ocelot.Text.Render rendered).addonLeft
    Test.Unit.test "`url` strips the scheme prefix of the input `String`" do
      Test.Unit.QuickCheck.quickCheck \appendPrefix value -> ado
        scheme <- Control.Monad.Gen.Common.genMaybe genScheme
        let
          prefixedValue :: String
          prefixedValue = case scheme of
            Nothing -> value
            Just scheme' ->
              if appendPrefix then
                addSchemePrefix scheme' value
              else
                value

          rendered :: Formlet.Ocelot.Text.Render (String -> String)
          rendered =
            Formlet.Render.match { text: map (un Data.Identity.Identity) }
              $ Formlet.render (Formlet.Ocelot.URL.url { scheme }) { readonly: false }
              $ prefixedValue

          expected :: String
          expected = value
        in expected === (un Formlet.Ocelot.Text.Render rendered).value
    Test.Unit.test "`url` produces a valid result for valid URL strings" do
      Test.Unit.QuickCheck.quickCheck ado
        uri <- genURI
        let
          value :: String
          value = uriToString uri

          expected :: Either Formlet.Errors (Maybe Formlet.Ocelot.URL.URI)
          expected = Right (Just uri)

          actual :: Either Formlet.Errors (Maybe Formlet.Ocelot.URL.URI)
          actual =
            Formlet.validate
              (Formlet.Ocelot.URL.url {} :: Formlet.Form _ _ Data.Identity.Identity _ _)
              { readonly: false }
              value
        in expected === actual
    Test.Unit.test "`url` does not produce a valid result for non-empty strings that cannot be parsed as a URL" do
      Test.Unit.QuickCheck.quickCheck \(value' :: Data.String.NonEmpty.NonEmptyString) ->
        let
          value :: String
          value = Data.String.NonEmpty.toString value'

          expected :: Boolean
          expected = Data.Either.isLeft (uriFromString value)

          actual :: Boolean
          actual =
            Data.Either.isLeft
              $ Formlet.validate
                  (Formlet.Ocelot.URL.url {} :: Formlet.Form _ _ Data.Identity.Identity _ _)
                  { readonly: false }
                  value
        in
          expected === actual

addSchemePrefix :: URI.Scheme -> String -> String
addSchemePrefix scheme string = Data.String.NonEmpty.toString (URI.Scheme.toString scheme) <> "://" <> string

genAlphaNum :: Test.QuickCheck.Gen.Gen String
genAlphaNum =
  map (Data.String.CodeUnits.fromCharArray <<< map (Data.Enum.toEnumWithDefaults bottom top))
    $ Test.QuickCheck.Gen.arrayOf
    $ Test.QuickCheck.Gen.oneOf
    $ pure (Test.QuickCheck.Gen.chooseInt 48 57)
    <> pure (Test.QuickCheck.Gen.chooseInt 65 90)
    <> pure (Test.QuickCheck.Gen.chooseInt 97 122)

genScheme :: Test.QuickCheck.Gen.Gen URI.Scheme
genScheme =
  -- We use `oneOf` here instead to select between a couple valid URI schemes,
  -- as the `uri` package doesn't provide a generator for them.
  Test.QuickCheck.Gen.oneOf
    $ pure (pure URI.Scheme.Common.ftp)
    <> pure (pure URI.Scheme.Common.http)
    <> pure (pure URI.Scheme.Common.https)

genURI :: Test.QuickCheck.Gen.Gen Formlet.Ocelot.URL.URI
genURI = ado
  hostPortPair <- URI.HostPortPair.Gen.genHostPortPair URI.Host.Gen.genHost URI.Port.Gen.genPort
  -- We unfortunately cannot use `Test.QuickCheck.Arbitrary.arbitrary :: Gen String`
  -- here to generate path segments, as that ends up causing pattern matching
  -- runtime errors, presumably because of invalid characters being generated.
  -- Because of that, we restrict ourselves to only alpha-numeric characters here.
  segments <- Test.QuickCheck.Gen.arrayOf genAlphaNum
  scheme <- genScheme
  let
    path :: URI.Path
    path = URI.Path (map URI.Path.Segment.segmentFromString segments)
  in URI.URI scheme (URI.HierarchicalPartAuth (URI.Authority Nothing hostPortPair) path) Nothing Nothing

uriFromString :: String -> Either Text.Parsing.Parser.ParseError Formlet.Ocelot.URL.URI
uriFromString str =
  Text.Parsing.Parser.runParser str
    $ URI.URI.parser
    $
      { parseFragment: pure
      , parseHierPath: pure
      , parseHosts: URI.HostPortPair.parser pure pure
      , parsePath: pure
      , parseQuery: pure
      , parseUserInfo: pure
      }

uriToString :: Formlet.Ocelot.URL.URI -> String
uriToString =
  URI.URI.print
    { printFragment: identity
    , printHierPath: identity
    , printHosts: URI.HostPortPair.print identity identity
    , printPath: identity
    , printQuery: identity
    , printUserInfo: identity
    }

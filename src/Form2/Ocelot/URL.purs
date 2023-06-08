module Formlet.Ocelot.URL
  ( Params
  , ParamsOptional
  , ParamsRequired
  , URI
  , url
  ) where

import CitizenNet.Prelude

import Data.Lens as Data.Lens
import Data.String as Data.String
import Data.String.NonEmpty as Data.String.NonEmpty
import Formlet as Formlet
import Formlet.Ocelot.Text as Formlet.Ocelot.Text
import Formlet.Render as Formlet.Render
import Formlet.Validation as Formlet.Validation
import Option as Option
import Text.Parsing.Parser as Text.Parsing.Parser
import URI as URI
import URI.HostPortPair as URI.HostPortPair
import URI.Scheme as URI.Scheme
import URI.URI as URI.URI

type Params =
  ( placeholder :: Maybe String
  , scheme :: Maybe URI.Scheme.Scheme
  )

type ParamsOptional =
  ( placeholder :: String
  , scheme :: URI.Scheme.Scheme
  )

type ParamsRequired =
  () :: Row Type

-- | A synonym to clean up the long type of `URI _ _ _ _ _ _ _`.
type URI = URI.URI URI.UserInfo (URI.HostPortPair.HostPortPair URI.Host URI.Port) URI.Path URI.HierPath URI.Query URI.Fragment

url ::
  forall config m options params renders.
  Applicative m =>
  Option.FromRecord params ParamsRequired ParamsOptional =>
  Record params ->
  Formlet.Form
    { readonly :: Boolean | config }
    ( Formlet.Render.Render
        (errors :: Formlet.Errors, required :: Boolean | options)
        (text :: Formlet.Ocelot.Text.Render | renders)
    )
    m
    String
    (Maybe URI)
url params' =
  Formlet.over isoStripSchemePrefix
    $ Formlet.Validation.validated (Formlet.Validation.optional Data.String.NonEmpty.fromString urlValidator)
    $ case schemePrefix of
        Nothing ->
          Formlet.Ocelot.Text.text
            { placeholder: params.placeholder
            }
        Just prefix ->
          Formlet.Ocelot.Text.text
            { addonLeft: Data.String.NonEmpty.toString prefix
            , placeholder: params.placeholder
            }
  where
  -- This partial `Iso'` takes care of removing the `scheme` from the `String`
  -- form value if it is a prefix in it. This ensures, for example, that pasting
  -- URLs with a matching scheme will not produce invalid URLs (e.g. `"https://https://citizennet.com"`).
  isoStripSchemePrefix :: Data.Lens.Iso' String String
  isoStripSchemePrefix = case schemePrefix of
    Nothing -> identity
    Just prefix'
      | prefix <- Data.String.NonEmpty.toString prefix' ->
          Data.Lens.iso
            (\string -> fromMaybe string (Data.String.stripPrefix (Data.String.Pattern prefix) string))
            identity

  params :: Record Params
  params =
    Option.recordToRecord
      ( Option.recordFromRecord params' ::
          Option.Record ParamsRequired ParamsOptional
      )

  schemePrefix :: Maybe Data.String.NonEmpty.NonEmptyString
  schemePrefix =
    map (_ <> Data.String.NonEmpty.nes (symbol { "://": _ }))
      $ map URI.Scheme.toString
      $ params.scheme

  uriFromString :: String -> Either Text.Parsing.Parser.ParseError URI
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

  urlValidator :: Formlet.Validation.Validator Data.String.NonEmpty.NonEmptyString URI
  urlValidator =
    Formlet.Validation.NotRequired \string ->
      -- Using `Text.Parsing.Parser.parseErrorMessage` here doesn't produce
      -- useful error messages, so we just stick to a simple message instead.
      lmap (\_ -> "Invalid URL")
        $ uriFromString
        $ Data.String.NonEmpty.toString
        $ maybe string (_ <> string) schemePrefix

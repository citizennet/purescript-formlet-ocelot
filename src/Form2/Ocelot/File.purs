module Form2.Ocelot.File
  ( Progress
  , Render(..)
  , Render'
  , URI
  , file
  , withRender
  ) where

import CitizenNet.Prelude

import Data.MediaType as Data.MediaType
import Form2 as Form2
import Form2.Render as Form2.Render
import URI as URI
import URI.HostPortPair as URI.HostPortPair
import Web.File.File as Web.File.File

type Progress
  -- The `lengthComputable` flag indicates whether the resource has a length
  -- that can be calculated. If `false`, the `total` has no significant value
  -- and the UI may choose to render an infinite loading indicator.
  =
  { lengthComputable :: Boolean
  , loaded :: Number
  , total :: Number
  }

-- We parametrize the File `Render` over the chosen uploaded `file` type but
-- hide it under an existential quantifier. This is so we can use the same
-- `Render` type for multiple different choices of `file`.
-- This representation is equivalent to `exists file. Render' file action`.
newtype Render action =
  Render (forall r. (forall file. Render' file action -> r) -> r)

type Render' file action =
  { download :: URI
  , getId :: file -> String
  , mediaTypes :: Array Data.MediaType.MediaType
  , onChange :: Maybe file -> action
  , readonly :: Boolean
  , upload :: (Progress -> Effect Unit) -> Web.File.File.File -> Aff (Either String file)
  , value :: Maybe file
  }

instance functorRender :: Functor Render where
  map f (Render k) =
    Render \cont ->
      k \render ->
        cont
          { download: render.download
          , getId: render.getId
          , mediaTypes: render.mediaTypes
          , onChange: map f render.onChange
          , readonly: render.readonly
          , upload: render.upload
          , value: render.value
          }

-- | A synonym to clean up the long type of `URI _ _ _ _ _ _ _`.
type URI = URI.URI URI.UserInfo (URI.HostPortPair.HostPortPair URI.Host URI.Port) URI.Path URI.HierPath URI.Query URI.Fragment

file ::
  forall config file options renders m.
  Applicative m =>
  -- The file id is appended to the `download` url when fetching uploaded files.
  { download :: URI
  -- Our implementation of the `file` form needs to store unique identifiers
  -- associated to the files internally, so we need a parameter like `getId`.
  , getId :: file -> String
  , mediaTypes :: Array Data.MediaType.MediaType
  , upload :: (Progress -> Effect Unit) -> Web.File.File.File -> Aff (Either String file)
  } ->
  Form2.Form
    { readonly :: Boolean
    | config
    }
    (Form2.Render.Render options (file :: Render | renders))
    m
    (Maybe file)
    (Maybe file)
file { download, getId, mediaTypes, upload } =
  Form2.form_ \{ readonly } value ->
    Form2.Render.inj
      { file:
          Render \cont ->
            cont
              { download
              , getId
              , mediaTypes
              , onChange: if readonly then const (pure identity) else pure <<< const
              , readonly
              , upload
              , value
              }
      }

-- | Unpack the existential quantification in `Render`, allowing us to use the
-- | hidden `file` type parameter as long as it doesn't escape the continuation.
withRender ::
  forall action r.
  Render action ->
  (forall file. Render' file action -> r) ->
  r
withRender (Render render) = render

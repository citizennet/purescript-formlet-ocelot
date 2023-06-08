module Formlet.Ocelot.File.Halogen
  ( Query(..)
  , Slot
  , Slots
  , render
  ) where

import CitizenNet.Prelude

import Data.MediaType as Data.MediaType
import Effect.Class.Console as Effect.Class.Console
import FilePond as FilePond
import FilePond.Halogen as FilePond.Halogen
import Formlet.Ocelot.File as Formlet.Ocelot.File
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.HTML.Properties as Halogen.HTML.Properties
import Web.File.File as Web.File.File

type Slots action slots =
  (upload :: Slot action Unit | slots)

render ::
  forall slots m config action.
  MonadAff m =>
  { readonly :: Boolean | config } ->
  Formlet.Ocelot.File.Render action ->
  Array (Halogen.ComponentHTML action (Slots action slots) m)
render { readonly } (Formlet.Ocelot.File.Render withRender) =
  [ withRender \render' ->
      Halogen.HTML.slot
        (Proxy :: Proxy "upload")
        unit
        component
        { download: render'.download
        , file: render'.value
        , getId: render'.getId
        , mediaTypes: render'.mediaTypes
        , onChange: render'.onChange
        , readonly: readonly || render'.readonly
        , upload: render'.upload
        }
        identity
  ]

-----------
-- Internal
-----------
data Action file output
  = HandleReceive (Input file output)
  | Initialize
  | HandleFilePond (FilePond.Event file)

type Component file output m =
  Halogen.Component Query (Input file output) output m

type ComponentHTML file output m =
  Halogen.ComponentHTML (Action file output) () m

type ComponentM file output m =
  Halogen.HalogenM (State file output) (Action file output) () output m

type Input file output =
  { download :: Formlet.Ocelot.File.URI
  , file :: Maybe file
  , getId :: file -> String
  , mediaTypes :: Array Data.MediaType.MediaType
  , onChange :: Maybe file -> output
  , readonly :: Boolean
  , upload :: (Formlet.Ocelot.File.Progress -> Effect Unit) -> Web.File.File.File -> Aff (Either String file)
  }

data Query (a :: Type)

type Slot output =
  Halogen.Slot Query output

type State file output =
  { input :: Input file output
  , filePond :: Maybe (FilePond.FilePond file)
  }

component ::
  forall file output m.
  MonadAff m =>
  Component file output m
component =
  Halogen.mkComponent
    { eval:
        Halogen.mkEval
          $ Halogen.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              , receive = \input -> Just (HandleReceive input)
              }
    , initialState
    , render: render'
    }
  where
  handleAction :: Action file output -> ComponentM file output m Unit
  handleAction = case _ of
    HandleReceive input -> handleReceive input
    Initialize -> do
      mElem <- Halogen.getHTMLElementRef refLabel
      state <- Halogen.get
      case mElem of
        Nothing -> pure unit
        Just elem -> do
          let
            options :: FilePond.Config file
            options =
              { disabled: state.input.readonly
              , download: state.input.download
              , file: state.input.file
              , getId: state.input.getId
              , mediaTypes: state.input.mediaTypes
              , upload: state.input.upload
              }
          filePond <- FilePond.Halogen.create options elem HandleFilePond
          Halogen.modify_ _ { filePond = Just filePond }
    HandleFilePond event -> do
      state <- Halogen.get
      case event of
        FilePond.Initialized -> Effect.Class.Console.log "[FilePond] Initialized"
        FilePond.WarningReceived { warning, status } -> Effect.Class.Console.log $ "[FilePond] Warning: " <> show { warning, status }
        FilePond.ErrorReceived { error, status } -> Effect.Class.Console.error $ "[FilePond] Error: " <> show { error, status }
        FilePond.AddFileStarted _ -> Effect.Class.Console.log "[FilePond] Start adding the file"
        FilePond.AddFileProgressed { progress } -> Effect.Class.Console.log $ "[FilePond] Adding file: " <> show progress <> "%"
        FilePond.FileAdded { error } -> Effect.Class.Console.log $ "[FilePond] File added: " <> show { error }
        FilePond.ProcessFileStarted _ -> Effect.Class.Console.log "[FilePond] Start processing the file"
        FilePond.ProcessFileProgressed { progress } -> Effect.Class.Console.log $ "[FilePond] Processing file: " <> show progress <> "%" -- TODO use Rational
        FilePond.ProcessFileAborted _ -> Effect.Class.Console.log "[FilePond] File processing aborted"
        FilePond.ProcessFileReverted _ -> Effect.Class.Console.log "[FilePond] Reverted file processing"
        FilePond.FileProcessed { error } -> Effect.Class.Console.log $ "[FilePond] File processed: " <> show { error }
        FilePond.AllFilesProcessed -> Effect.Class.Console.log "[FilePond] All files have been successfully processed"
        FilePond.FileRemoved _ -> Halogen.raise (state.input.onChange Nothing)
        FilePond.FilePrepared _ -> Effect.Class.Console.log "[FilePond] File is prepared"
        FilePond.FilesUpdated _ -> Effect.Class.Console.log "[FilePond] File list has been updated"
        FilePond.FileActivated _ -> Effect.Class.Console.log "[FilePond] File is activated"
        FilePond.FilesReordered _ -> Effect.Class.Console.log "[FilePond] File list has been reordered"
        FilePond.UploadResponse file -> Halogen.raise (state.input.onChange (Just file))

  handleReceive :: Input file output -> ComponentM file output m Unit
  handleReceive input = do
    state <- Halogen.modify _ { input = input }
    liftEffect case state.filePond of
      Nothing -> pure unit
      Just filePond -> do
        FilePond.setDisabled filePond input.readonly

  initialState :: Input file output -> State file output
  initialState input =
    { input
    , filePond: Nothing
    }

  refLabel :: Halogen.RefLabel
  refLabel = Halogen.RefLabel "image-upload"

  render' :: State file output -> ComponentHTML file output m
  render' _ =
    Halogen.HTML.div
      [ Halogen.HTML.Properties.ref refLabel
      ]
      []

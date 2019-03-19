module TeamTavern.Client.EditGame where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.Foldable (foldl)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.String (trim)
import Data.Variant (SProxy(..), match)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Game.Update.SendResponse as Update
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input =
    { title :: String
    , handle :: String
    , description :: String
    }

data Action
    = TitleInput String
    | HandleInput String
    | DescriptionInput String
    | Update Event

data Message = GameUpdated String

type State =
    { originalTitle :: String
    , originalHandle :: String
    , title :: String
    , handle :: String
    , description :: String
    , titleError :: Boolean
    , handleError :: Boolean
    , descriptionError :: Boolean
    , titleTaken :: Boolean
    , handleTaken :: Boolean
    , otherError :: Boolean
    }

type Slot = H.Slot (Modal.Query (Const Void)) (Modal.Message Message)

render :: forall slots. State -> HH.HTML slots Action
render
    { originalTitle
    , title
    , handle
    , description
    , titleError
    , handleError
    , descriptionError
    , titleTaken
    , handleTaken
    , otherError
    } = HH.form
    [ HP.class_ $ H.ClassName "single-form", HE.onSubmit $ Just <<< Update ]
    [ HH.h2_ [ HH.text $ "Edit " <> originalTitle ]
    , HH.div_
        [ HH.label
            [ HP.for "title" ]
            [ HH.text "Title" ]
        , HH.input
            [ HP.id_ "title"
            , HE.onValueInput $ Just <<< TitleInput
            , HP.value title
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass titleError ]
            [ HH.text "The title cannot be more than 50 characters long." ]
        , HH.p
            [ HP.class_ $ inputErrorClass titleTaken ]
            [ HH.text "This title is already taken, please pick another one." ]
        ]
    , HH.div_
        [ HH.label
            [ HP.for "handle" ]
            [ HH.text "Handle" ]
        , HH.input
            [ HP.id_ "handle"
            , HE.onValueInput $ Just <<< HandleInput
            , HP.value handle
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass handleError ]
            [ HH.text
                $  "The handle can contain only alphanumeric characters and "
                <> "cannot be more than 50 characters long." ]
        , HH.p
            [ HP.class_ $ inputErrorClass handleTaken ]
            [ HH.text "This handle is already taken, please pick another one." ]
        ]
    , HH.div_
        [ HH.label
            [ HP.for "description" ]
            [ HH.text "Description" ]
        , HH.textarea
            [ HP.id_ "description"
            , HE.onValueInput $ Just <<< DescriptionInput
            , HP.value description
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass descriptionError ]
            [ HH.text
                "The description cannot be more than 2000 characters long." ]
        ]
    , HH.button
        [ HP.class_ $ ClassName "primary"
        , HP.disabled $ title == "" || handle == "" || description == ""
        ]
        [ HH.text "Save changes" ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    ]

updateGame :: forall left. State -> Async left (Maybe State)
updateGame state @ { title, handle, description, originalHandle } =
    Async.unify do
    response <- Fetch.fetch ("/api/games/by-handle/" <> originalHandle)
        (  Fetch.method := PUT
        <> Fetch.body := Json.writeJSON { title, handle, description }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true})
                (\(error :: Update.BadRequestContent) -> Just $ match
                    { invalidDetails: foldl (\state' -> match
                        { invalidTitle:
                            const $ state' { titleError = true }
                        , invalidHandle:
                            const $ state' { handleError = true }
                        , invalidDescription:
                            const $ state' { descriptionError = true }
                        })
                        state
                    , titleTaken: const $ state { titleTaken = true }
                    , handleTaken: const $ state { handleTaken = true }
                    }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

handleAction :: forall slots left.
    Action -> H.HalogenM State Action slots Message (Async left) Unit
handleAction (TitleInput title) =
    H.modify_ (_ { title = title }) $> unit
handleAction (HandleInput handle) =
    H.modify_ (_ { handle = handle }) $> unit
handleAction (DescriptionInput description) =
    H.modify_ (_ { description = description }) $> unit
handleAction (Update event) = do
    H.liftEffect $ preventDefault event
    state <- H.get
    newState <- H.lift $ updateGame $ state
        { titleError       = false
        , handleError      = false
        , descriptionError = false
        , titleTaken       = false
        , handleTaken      = false
        , otherError       = false
        }
    case newState of
        Nothing -> H.raise $ GameUpdated $ trim state.handle
        Just newState' -> H.put newState'
    pure unit

component :: forall query input left.
    Input -> H.Component HH.HTML query input Message (Async left)
component { title, handle, description } = H.mkComponent
    { initialState: const
        { originalTitle: title
        , originalHandle: handle
        , title
        , handle
        , description
        , titleError: false
        , handleError: false
        , descriptionError: false
        , titleTaken: false
        , handleTaken: false
        , otherError: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

editGame
    :: forall query children left
    .  Input
    -> (Modal.Message Message -> Maybe query)
    -> HH.ComponentHTML query (editGame :: Slot Unit | children) (Async left)
editGame input handleMessage = HH.slot
    (SProxy :: SProxy "editGame") unit
    (Modal.component $ component input) unit handleMessage

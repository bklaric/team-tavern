module TeamTavern.Client.CreateGame where

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
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Game.Create.SendResponse as Create
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data Action
    = TitleInput String
    | HandleInput String
    | DescriptionInput String
    | Create Event

data Message = GameCreated

type State =
    { title :: String
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
    { title
    , handle
    , description
    , titleError
    , handleError
    , descriptionError
    , titleTaken
    , handleTaken
    , otherError
    } = HH.form
    [ HP.class_ $ ClassName "single-form", HE.onSubmit $ Just <<< Create ]
    [ HH.h2_ [ HH.text "Create a new game" ]
    , HH.div_
        [ HH.label
            [ HP.for "title" ]
            [ HH.text "Title" ]
        , HH.input
            [ HP.id_ "title"
            , HE.onValueInput $ Just <<< TitleInput
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
        [ HH.text "Create" ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    ]

sendCreateRequest :: forall left. State -> Async left (Maybe State)
sendCreateRequest state @ { title, handle, description } = Async.unify do
    response <- Fetch.fetch "/api/games"
        (  Fetch.method := POST
        <> Fetch.body := Json.writeJSON { title, handle, description }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true })
                (\(error :: Create.BadRequestContent) -> Just $ match
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
    H.modify_ (_ { title = title }) <#> const unit
handleAction (HandleInput handle) =
    H.modify_ (_ { handle = handle }) <#> const unit
handleAction (DescriptionInput description) =
    H.modify_ (_ { description = description }) <#> const unit
handleAction (Create event) = do
    H.liftEffect $ preventDefault event
    state <- H.gets (_
        { titleError       = false
        , handleError      = false
        , descriptionError = false
        , titleTaken       = false
        , handleTaken      = false
        , otherError       = false
        })
    newState <- H.lift $ sendCreateRequest state
    case newState of
        Nothing -> do
            H.raise GameCreated
            H.liftEffect $ navigate_ $ "/games/" <> trim state.handle
        Just newState' -> H.put newState'
    pure unit

component :: forall query input left.
    H.Component HH.HTML query input Message (Async left)
component = H.mkComponent
    { initialState: const
        { title: ""
        , handle: ""
        , description: ""
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

createGame
    :: forall action children left
    .  (Modal.Message Message -> Maybe action)
    -> HH.ComponentHTML action (createGame :: Slot Unit | children) (Async left)
createGame handleMessage = HH.slot
    (SProxy :: SProxy "createGame") unit
    (Modal.component component) unit handleMessage

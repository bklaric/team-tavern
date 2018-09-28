module TeamTavern.Client.CreateGame (Query, Slot, createGame) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (bimap, lmap)
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
import TeamTavern.Client.Script.Cookie (hasPlayerIdCookie)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Snippets.ErrorClasses (errorClass, inputErrorClass, otherErrorClass)
import TeamTavern.Game.Create.Response as Create
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data Query send
    = Init send
    | TitleInput String send
    | HandleInput String send
    | DescriptionInput String send
    | Create Event send

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

type Slot = H.Slot Query Void

render :: forall left. State -> H.ComponentHTML Query () (Async left)
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
    [ HE.onSubmit $ HE.input Create ]
    [ HH.h2_ [ HH.text "Create a new game" ]
    , HH.div_
        [ HH.label
            [ HP.class_ $ errorClass titleError, HP.for "title" ]
            [ HH.text "Title" ]
        , HH.input
            [ HP.id_ "title"
            , HP.class_ $ errorClass titleError
            , HE.onValueInput $ HE.input TitleInput
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass titleError ]
            [ HH.text "Please enter a valid title. The title must: " ]
        , HH.ul
            [ HP.class_ $ inputErrorClass titleError ]
            [ HH.li_ [ HH.text "Have no spaces" ]
            , HH.li_ [ HH.text "Be some shit I forgot" ]
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass titleTaken ]
            [ HH.text "This title is taken, pick another one." ]
        ]
    , HH.div_
        [ HH.label
            [ HP.class_ $ errorClass handleError, HP.for "handle" ]
            [ HH.text "Handle" ]
        , HH.input
            [ HP.id_ "handle"
            , HP.class_ $ errorClass handleError
            , HE.onValueInput $ HE.input HandleInput
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass handleError ]
            [ HH.text "This does not look like a valid handle. Jesus Christ, how dense are you?" ]
        , HH.p
            [ HP.class_ $ inputErrorClass handleTaken ]
            [ HH.text "This handle is taken, pick another one." ]
        ]
    , HH.div_
        [ HH.label
            [ HP.class_ $ errorClass descriptionError, HP.for "description" ]
            [ HH.text "Description" ]
        , HH.textarea
            [ HP.id_ "description"
            , HP.class_ $ errorClass descriptionError
            , HE.onValueInput $ HE.input DescriptionInput
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass descriptionError ]
            [ HH.text "This does not look like a valid description. Jesus Christ, how dense are you?" ]
        ]
    , HH.button
        [ HP.class_ $ ClassName "primary"
        , HP.disabled $ title == "" || handle == "" || description == ""
        ]
        [ HH.text "Create" ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Lmao, something else got fucked and you're shit out of luck, mate!"]
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

eval :: forall void.
    Query ~> H.HalogenM State Query () Void (Async void)
eval (Init send) = do
    isSignedIn <- H.liftEffect hasPlayerIdCookie
    if isSignedIn
        then pure unit
        else H.liftEffect $ navigate_ "/"
    pure send
eval (TitleInput title send) =
    H.modify_ (_ { title = title }) <#> const send
eval (HandleInput handle send) =
    H.modify_ (_ { handle = handle }) <#> const send
eval (DescriptionInput description send) =
    H.modify_ (_ { description = description }) <#> const send
eval (Create event send) = do
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
        Nothing -> H.liftEffect $ navigate_ $ "/games/" <> trim state.handle
        Just newState' -> H.put newState'
    pure send

component :: forall input left.
    H.Component HH.HTML Query input Void (Async left)
component = H.component
    { initialState: const
        { title: ""
        , handle: ""
        , description: ""
        , titleError : false
        , handleError : false
        , descriptionError : false
        , titleTaken : false
        , handleTaken : false
        , otherError : false
        }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Init
    , finalizer: Nothing
    }

createGame :: forall query children left.
    HH.ComponentHTML query (createGame :: Slot Unit | children) (Async left)
createGame = HH.slot (SProxy :: SProxy "createGame") unit component unit absurd

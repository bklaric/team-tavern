module TeamTavern.Client.EditGame (Query, Slot, editGame) where

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
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy(..), match)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Script.Cookie (getPlayerInfo, hasPlayerIdCookie)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Snippets.ErrorClasses (errorClass, inputErrorClass, otherErrorClass)
import TeamTavern.Game.Update.Response as Update
import TeamTavern.Game.View.Response as View
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data Query send
    = Init String send
    | TitleInput String send
    | HandleInput String send
    | DescriptionInput String send
    | Update Event send

type State' =
    { administratorId :: Int
    , originalTitle :: String
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

data State
    = Empty
    | Game State'
    | NotFound
    | Error

type Slot = H.Slot Query Void

render :: forall left. State -> H.ComponentHTML Query () (Async left)
render Empty = HH.div_ []
render (Game
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
    }) = HH.form
    [ HE.onSubmit $ HE.input Update ]
    [ HH.h2_ [ HH.text $ "Edit " <> originalTitle ]
    , HH.div_
        [ HH.label
            [ HP.class_ $ errorClass titleError, HP.for "title" ]
            [ HH.text "Title" ]
        , HH.input
            [ HP.id_ "title"
            , HP.class_ $ errorClass titleError
            , HE.onValueInput $ HE.input TitleInput
            , HP.value title
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
            , HP.value handle
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
            , HP.value description
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass descriptionError ]
            [ HH.text "This does not look like a valid description. Jesus Christ, how dense are you?" ]
        ]
    , HH.button
        [ HP.class_ $ ClassName "primary"
        , HP.disabled $ title == "" || handle == "" || description == ""
        ]
        [ HH.text "Save changes" ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Lmao, something else got fucked and you're shit out of luck, mate!"]
    ]
render NotFound = HH.p_ [ HH.text "Game could not be found." ]
render Error = HH.p_ [ HH.text
    "There has been an error loading the game. Please try again later." ]

loadGame :: forall left. String -> Async left State
loadGame handle' = Async.unify do
    response <- Fetch.fetch_ ("/api/games/" <> handle') # lmap (const Error)
    { administratorId, title, handle, description } :: View.OkContent <-
        case FetchRes.status response of
        200 -> FetchRes.text response
            >>= JsonAsync.readJSON # lmap (const Error)
        404 -> Async.left NotFound
        _ -> Async.left Error
    pure $ Game
        { administratorId
        , originalTitle: title
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

updateGame :: forall left. State' -> Async left (Maybe State')
updateGame state @ { title, handle, description, originalHandle } =
    Async.unify do
    response <- Fetch.fetch ("/api/games/" <> originalHandle)
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

eval :: forall left. Query ~> H.HalogenM State Query () Void (Async left)
eval (Init handle send) = do
    isSignedIn <- H.liftEffect hasPlayerIdCookie
    if isSignedIn
        then do
            state <- H.lift $ loadGame handle
            playerInfo <- H.lift $ Async.fromEffect getPlayerInfo
            case (Tuple state playerInfo) of
                Tuple (Game { administratorId }) (Just { id }) ->
                    if administratorId == id
                    then H.put state
                    else H.liftEffect $ navigate_ "/"
                _ -> H.liftEffect $ navigate_ "/"
        else H.liftEffect $ navigate_ "/"
    pure send
eval (TitleInput title send) = do
    H.modify_ $ case _ of
        Game state -> Game $ state { title = title }
        state -> state
    pure send
eval (HandleInput handle send) = do
    H.modify_ $ case _ of
        Game state -> Game $ state { handle = handle }
        state -> state
    pure send
eval (DescriptionInput description send) = do
    H.modify_ $ case _ of
        Game state -> Game $ state { description = description }
        state -> state
    pure send
eval (Update event send) = do
    H.liftEffect $ preventDefault event
    state <- H.get
    case state of
        Game state' -> do
            newState <- H.lift $ updateGame $ state'
                { titleError       = false
                , handleError      = false
                , descriptionError = false
                , titleTaken       = false
                , handleTaken      = false
                , otherError       = false
                }
            case newState of
                Nothing -> H.liftEffect $ navigate_ $
                    "/games/" <> trim state'.handle
                Just newState' -> H.put $ Game newState'
        _ -> pure unit
    pure send

component :: forall input left.
    String -> H.Component HH.HTML Query input Void (Async left)
component handle = H.component
    { initialState: const Empty
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action $ Init handle
    , finalizer: Nothing
    }

editGame
    :: forall query children left
    .  String
    -> HH.ComponentHTML query (editGame :: Slot Unit | children) (Async left)
editGame handle =
    HH.slot (SProxy :: SProxy "editGame") unit (component handle) unit absurd

module TeamTavern.Client.EditPlayer where

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
import TeamTavern.Client.Script.Cookie (getPlayerId, hasPlayerIdCookie)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Snippets.ErrorClasses (errorClass, inputErrorClass, otherErrorClass)
import TeamTavern.Player.Update.SendResponse as Update
import TeamTavern.Player.View.SendResponse as View
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)
import Web.HTML (window)
import Web.HTML.Location (setHref)
import Web.HTML.Window (location)

data Query send
    = Init String send
    | NicknameInput String send
    | AboutInput String send
    | Update Event send

type PlayerState =
    { id :: Int
    , originalNickname :: String
    , nickname :: String
    , about :: String
    , nicknameError :: Boolean
    , aboutError :: Boolean
    , nicknameTaken :: Boolean
    , otherError :: Boolean
    }

data State
    = Empty
    | Player PlayerState
    | NotFound
    | Error

type Slot = H.Slot Query Void

render :: forall left. State -> H.ComponentHTML Query () (Async left)
render Empty = HH.div_ []
render (Player
    { originalNickname
    , nickname
    , about
    , nicknameError
    , aboutError
    , nicknameTaken
    , otherError
    }) = HH.form
    [ HE.onSubmit $ HE.input Update ]
    [ HH.h2_ [ HH.text "Edit info" ]
    , HH.div_
        [ HH.label
            [ HP.class_ $ errorClass nicknameError, HP.for "nickname" ]
            [ HH.text "Nickname" ]
        , HH.input
            [ HP.id_ "nickname"
            , HP.class_ $ errorClass nicknameError
            , HE.onValueInput $ HE.input NicknameInput
            , HP.value nickname
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass nicknameError ]
            [ HH.text "Please enter a valid nickname. The nickname must: " ]
        , HH.ul
            [ HP.class_ $ inputErrorClass nicknameError ]
            [ HH.li_ [ HH.text "Contain only alphanumeric characters." ]
            , HH.li_ [ HH.text "Be no more than 40 characters long." ]
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass nicknameTaken ]
            [ HH.text "This nickname is taken, please pick another one." ]
        ]
    , HH.div_
        [ HH.label
            [ HP.class_ $ errorClass aboutError, HP.for "about" ]
            [ HH.text "About" ]
        , HH.textarea
            [ HP.id_ "about"
            , HP.class_ $ errorClass aboutError
            , HE.onValueInput $ HE.input AboutInput
            , HP.value about
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass aboutError ]
            [ HH.text
                "The about entry cannot be more than 2000 characters long." ]
        ]
    , HH.button
        [ HP.class_ $ ClassName "primary"
        , HP.disabled $ nickname == "" || about == ""
        ]
        [ HH.text "Save changes" ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    ]
render NotFound = HH.p_ [ HH.text "Game could not be found." ]
render Error = HH.p_ [ HH.text
    "There has been an error loading the player. Please try again later." ]

loadPlayer :: forall left. String -> Async left State
loadPlayer nickname' = Async.unify do
    response <- Fetch.fetch_ ("/api/players/by-nickname/" <> nickname') # lmap (const Error)
    { id, nickname, about } :: View.OkContent <- case FetchRes.status response of
        200 -> FetchRes.text response >>= JsonAsync.readJSON # lmap (const Error)
        404 -> Async.left NotFound
        _ -> Async.left Error
    pure $ Player
        { id
        , originalNickname: nickname
        , nickname
        , about
        , nicknameError: false
        , aboutError: false
        , nicknameTaken: false
        , otherError: false
        }

updatePlayer :: forall left. PlayerState -> Async left (Maybe PlayerState)
updatePlayer state @ { originalNickname, nickname, about } = Async.unify do
    response <- Fetch.fetch ("/api/players/by-nickname/" <> originalNickname)
        (  Fetch.method := PUT
        <> Fetch.body := Json.writeJSON { nickname, about }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true})
                (\(error :: Update.BadRequestContent) -> Just $ match
                    { invalidIdentifiers: foldl (\state' -> match
                        { invalidNickname:
                            const $ state' { nicknameError = true }
                        , invalidAbout:
                            const $ state' { aboutError = true }
                        })
                        state
                    , nicknameTaken: const $ state { nicknameTaken = true }
                    }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

eval :: forall left. Query ~> H.HalogenM State Query () Void (Async left)
eval (Init nickname send) = do
    isSignedIn <- H.liftEffect hasPlayerIdCookie
    if isSignedIn
        then do
            state <- H.lift $ loadPlayer nickname
            playerId <- H.lift $ Async.fromEffect getPlayerId
            case (Tuple state playerId) of
                Tuple (Player playerState) (Just playerId') ->
                    if playerState.id == playerId'
                    then H.put state
                    else H.liftEffect $ navigate_ "/"
                _ -> H.liftEffect $ navigate_ "/"
        else H.liftEffect $ navigate_ "/"
    pure send
eval (NicknameInput nickname send) = do
    H.modify_ $ case _ of
        Player state -> Player $ state { nickname = nickname }
        state -> state
    pure send
eval (AboutInput about send) = do
    H.modify_ $ case _ of
        Player state -> Player $ state { about = about }
        state -> state
    pure send
eval (Update event send) = do
    H.liftEffect $ preventDefault event
    state <- H.get
    case state of
        Player state' -> do
            newState <- H.lift $ updatePlayer $ state'
                { nicknameError = false
                , aboutError    = false
                , nicknameTaken = false
                , otherError    = false
                }
            case newState of
                Nothing -> H.liftEffect $
                    window
                    >>= location
                    >>= (setHref $ "/players/" <> trim state'.nickname)
                Just newState' -> H.put $ Player newState'
        _ -> pure unit
    pure send

component :: forall input left.
    String -> H.Component HH.HTML Query input Void (Async left)
component nickname = H.component
    { initialState: const Empty
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action $ Init nickname
    , finalizer: Nothing
    }

editPlayer
    :: forall query children left
    .  String
    -> HH.ComponentHTML query (editPlayer :: Slot Unit | children) (Async left)
editPlayer nickname =
    HH.slot (SProxy :: SProxy "editPlayer") unit (component nickname) unit absurd

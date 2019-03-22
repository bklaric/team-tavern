module TeamTavern.Client.EditPlayer where

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
import TeamTavern.Player.Update.SendResponse as Update
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input =
    { nickname :: String
    , about :: String
    }

data Action
    = NicknameInput String
    | AboutInput String
    | Update Event

data Message = PlayerUpdated String

type State =
    { originalNickname :: String
    , nickname :: String
    , about :: String
    , nicknameError :: Boolean
    , aboutError :: Boolean
    , nicknameTaken :: Boolean
    , otherError :: Boolean
    }

type Slot = H.Slot (Modal.Query Unit (Const Void)) (Modal.Message Message)

render :: forall slots. State -> HH.HTML slots Action
render
    { originalNickname
    , nickname
    , about
    , nicknameError
    , aboutError
    , nicknameTaken
    , otherError
    } = HH.form
    [ HP.class_ $ H.ClassName "single-form", HE.onSubmit $ Just <<< Update ]
    [ HH.h2_ [ HH.text "Edit info" ]
    , HH.div_
        [ HH.label
            [ HP.for "nickname" ]
            [ HH.text "Nickname" ]
        , HH.input
            [ HP.id_ "nickname"
            , HE.onValueInput $ Just <<< NicknameInput
            , HP.value nickname
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass nicknameError ]
            [ HH.text
                $ "The nickname can contain only alphanumeric characters and "
                <> "cannot be more than 40 characters long." ]
        , HH.p
            [ HP.class_ $ inputErrorClass nicknameTaken ]
            [ HH.text
                "This nickname is already taken, please pick another one." ]
        ]
    , HH.div_
        [ HH.label
            [ HP.for "about" ]
            [ HH.text "About" ]
        , HH.textarea
            [ HP.id_ "about"
            , HE.onValueInput $ Just <<< AboutInput
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

updatePlayer :: forall left. State -> Async left (Maybe State)
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

handleAction :: forall slots left.
    Action -> H.HalogenM State Action slots Message (Async left) Unit
handleAction (NicknameInput nickname) =
    H.modify_ (_ { nickname = nickname }) $> unit
handleAction (AboutInput about) = do
    H.modify_ (_ { about = about }) $> unit
handleAction (Update event) = do
    H.liftEffect $ preventDefault event
    state <- H.gets (_
        { nicknameError = false
        , aboutError    = false
        , nicknameTaken = false
        , otherError    = false
        })
    newState <- H.lift $ updatePlayer state
    case newState of
        Nothing -> H.raise $ PlayerUpdated $ trim state.nickname
        Just newState' -> H.put newState'
    pure unit

component :: forall query input left.
    Input -> H.Component HH.HTML query input Message (Async left)
component { nickname, about } = H.mkComponent
    { initialState: const
        { originalNickname: nickname
        , nickname
        , about
        , nicknameError: false
        , aboutError: false
        , nicknameTaken: false
        , otherError: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

editPlayer
    :: forall query children left
    .  Input
    -> (Modal.Message Message -> Maybe query)
    -> HH.ComponentHTML query (editPlayer :: Slot Unit | children) (Async left)
editPlayer input handleMessage = HH.slot
    (SProxy :: SProxy "editPlayer") unit
    (Modal.component $ component input) unit handleMessage

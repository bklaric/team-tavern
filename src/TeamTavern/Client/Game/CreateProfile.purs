module TeamTavern.Client.Game.CreateProfile where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Options ((:=))
import Data.String (null)
import Data.Variant (SProxy(..), match)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Script.Cookie (getPlayerNickname)
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Server.Game.View.SendResponse as View
import TeamTavern.Server.Profile.Create.SendResponse as Create
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input = { handle :: String }

data Action
    = SummaryInput String
    | UrlValueInput Int String
    | Create Event

data Message = ProfileCreated String

type State =
    { game :: View.OkContent
    , summary :: String
    , summaryError :: Boolean
    , fieldValues :: Array
        { fieldId :: Int
        , url :: Maybe String
        , optionId :: Maybe Int
        , optionIds :: Maybe (Array Int)
        }
    , otherError :: Boolean
    }

type Slot =
    H.Slot (Modal.Query View.OkContent (Const Void)) (Modal.Message Message)

fieldInput { id, type: 1, label } = HH.div_
    [ HH.label [ HP.for label ] [ HH.text label ]
    , HH.input
        [ HP.id_ label
        , HE.onValueInput $ Just <<< UrlValueInput id
        ]
    ]
-- fieldInput { id, type: 2, label, options } = 3
-- fieldInput { id, type: 3, label, options } = 4
fieldInput _ = HH.div_ []

render :: forall slots. State -> HH.HTML slots Action
render { summary, summaryError, otherError, game } = HH.form
    [ HP.class_ $ ClassName "single-form-wide", HE.onSubmit $ Just <<< Create ] $
    [ HH.h2_ [ HH.text "Create a new profile" ] ]
    <> (game.fields <#> fieldInput) <>
    [ HH.div_
        [ HH.label
            [ HP.for "summary" ]
            [ HH.text "Summary" ]
        , HH.textarea
            [ HP.id_ "summary"
            , HE.onValueInput $ Just <<< SummaryInput
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass summaryError ]
            [ HH.text
                "The summary cannot be more than 2000 characters long." ]
        ]
    , HH.button
        [ HP.class_ $ ClassName "primary"
        , HP.disabled $ summary == ""
        ]
        [ HH.text "Create" ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    ]

sendCreateRequest :: forall left. State -> String -> Async left (Maybe State)
sendCreateRequest state @ { summary, fieldValues, game } nickname = Async.unify do
    response <- Fetch.fetch
        ("/api/games/by-handle/" <> game.handle <> "/profiles/by-nickname/" <> nickname)
        (  Fetch.method := POST
        <> Fetch.body := Json.writeJSON
            { summary
            , fieldValues: fieldValues # Array.filter
                \{ url, optionId, optionIds } ->
                    isJust url || isJust optionId || isJust optionIds
            }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true })
                (\(error :: Create.BadRequestContent) -> Just $ match
                    { invalidSummary: const $ state { summaryError = true }
                    }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

handleAction :: forall slots left.
    Action -> H.HalogenM State Action slots Message (Async left) Unit
handleAction (SummaryInput summary) =
    H.modify_ (_ { summary = summary }) $> unit
handleAction (UrlValueInput fieldId url) = do
    state @ { fieldValues } <- H.get
    let newFieldValues = fieldValues <#> \value ->
        if value.fieldId == fieldId
            then
                if null url
                then value { url = Nothing }
                else value { url = Just url }
            else value
    H.put $ state { fieldValues = newFieldValues }
handleAction (Create event) = do
    H.liftEffect $ preventDefault event
    state <- H.gets (_
        { summaryError = false
        , otherError   = false
        })
    nickname <- H.liftEffect getPlayerNickname
    case nickname of
        Nothing -> H.put $ state { otherError = true }
        Just nickname' -> do
            newState <- H.lift $ sendCreateRequest state nickname'
            case newState of
                Nothing -> H.raise $ ProfileCreated state.game.handle
                Just newState' -> H.put newState'
            pure unit

component :: forall query left.
    H.Component HH.HTML query View.OkContent Message (Async left)
component = H.mkComponent
    { initialState: \game ->
        { summary: ""
        , summaryError: false
        , fieldValues: game.fields <#> (\field ->
            { fieldId: field.id
            , url: Nothing
            , optionId: Nothing
            , optionIds: Nothing
            })
        , otherError: false
        , game
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

createProfile
    :: forall query children left
    .  (Modal.Message Message -> Maybe query)
    -> HH.ComponentHTML query
        (createProfile :: Slot Unit | children)
        (Async left)
createProfile handleMessage = HH.slot
    (SProxy :: SProxy "createProfile") unit
    (Modal.component component) unit handleMessage

module TeamTavern.Client.Game.CreateProfile where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (any, foldl)
import Data.Array as Arary
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.HTTP.Method (Method(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Options ((:=))
import Data.String (null)
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy(..), match)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.SingleSelect (singleSelectIndexed)
import TeamTavern.Client.Components.SingleSelect as SingleSelect
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
    , urlValueErrors :: Array { fieldId :: Int }
    , otherError :: Boolean
    }

type ChildSlots = ("singleSelectField" :: SingleSelect.Slot Int)

type Slot =
    H.Slot (Modal.Query View.OkContent (Const Void)) (Modal.Message Message)

fieldInput
    :: forall left
    .  Array { fieldId :: Int }
    ->  { id :: Int
        , label :: String
        , type :: Int
        , options :: Maybe (Array { id :: Int , option :: String })
        }
    ->  H.ComponentHTML Action ChildSlots (Async left)
fieldInput urlValueErrors { id, type: 1, label } = let
    urlError = urlValueErrors # any (_.fieldId >>> (_ == id))
    in
    HH.div_
    [ HH.label [ HP.for label ] [ HH.text label ]
    , HH.input
        [ HP.id_ label
        , HE.onValueInput $ Just <<< UrlValueInput id
        ]
    , HH.p
        [ HP.class_ $ inputErrorClass urlError ]
        [ HH.text "This doesn't look like a valid web address." ]
    ]
fieldInput _ { id, type: 2, label, options: Just options } =
    HH.div_
    [ HH.label [ HP.for label ] [ HH.text label ]
    , singleSelectIndexed (SProxy :: SProxy "singleSelectField") id
        { options, selectedId: Nothing }
    ]
-- fieldInput { id, type: 3, label, options } = 4
fieldInput _ _ = HH.div_ []

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { summary, summaryError, urlValueErrors, otherError, game } = HH.form
    [ HP.class_ $ ClassName "single-form-wide", HE.onSubmit $ Just <<< Create ] $
    [ HH.h2_ [ HH.text "Create a new profile" ] ]
    <> (game.fields <#> fieldInput urlValueErrors) <>
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
                    { invalidProfile: foldl (\state' error' ->
                        error' # match
                            { invalidSummary: const $ state' { summaryError = true }
                            , invalidUrl: \fieldId ->
                                state' { urlValueErrors = Arary.cons fieldId state'.urlValueErrors }
                            })
                        state
                    }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Message (Async left) Unit
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
    state @ { game: { fields } } <- H.gets (_
        { summaryError = false
        , otherError   = false
        })
    singleSelectResults <-
        (H.queryAll (SProxy :: SProxy "singleSelectField")
        $ SingleSelect.Selected identity)
    let (singleSelectValues :: Array _) =
            singleSelectResults
            # Map.toUnfoldable
            <#> \(Tuple fieldId option) ->
                { fieldId
                , url: Nothing
                , optionId: option <#> _.id
                , optionIds: Nothing
                }
    let state' = state { fieldValues = state.fieldValues <> singleSelectValues }
    nickname <- H.liftEffect getPlayerNickname
    case nickname of
        Nothing -> H.put $ state { otherError = true }
        Just nickname' -> do
            newState <- H.lift $ sendCreateRequest state' nickname'
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
        , urlValueErrors: []
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

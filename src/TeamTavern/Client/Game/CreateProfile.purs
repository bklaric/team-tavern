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
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.MultiSelect (multiSelectIndexed)
import TeamTavern.Client.Components.MultiSelect as MultiSelect
import TeamTavern.Client.Components.SingleSelect (singleSelectIndexed)
import TeamTavern.Client.Components.SingleSelect as SingleSelect
import TeamTavern.Client.Script.Cookie (PlayerInfo)
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Server.Game.View.SendResponse as View
import TeamTavern.Server.Profile.Create.SendResponse as Create
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Input = { game :: View.OkContent, playerInfo :: PlayerInfo }

data Action
    = SummaryInput String
    | UrlValueInput String String
    | Create Event

data Message = ProfileCreated String

type State =
    { game :: View.OkContent
    , playerInfo :: PlayerInfo
    , summary :: String
    , summaryError :: Boolean
    , fieldValues :: Array
        { fieldKey :: String
        , url :: Maybe String
        , optionKey :: Maybe String
        , optionKeys :: Maybe (Array String)
        }
    , urlValueErrors :: Array { fieldKey :: String }
    , missingErrors :: Array { fieldKey :: String }
    , otherError :: Boolean
    }

type ChildSlots =
    ( "singleSelectField" :: SingleSelect.Slot { key :: String, option :: String } String
    , "multiSelectField" :: MultiSelect.Slot { key :: String, option :: String } String
    )

type Slot =
    H.Slot (Modal.Query Input (Const Void)) (Modal.Message Message)

fieldLabel :: forall slots action. String -> String -> Boolean -> Maybe String -> HH.HTML slots action
fieldLabel key label required domain =
    HH.label
        [ HP.class_ $ HH.ClassName "input-label", HP.for key ] $
        [ HH.text label ]
        <>
        (case domain of
        Just domain' ->
            [ divider
            , HH.span [ HP.class_ $ H.ClassName "profile-count" ] [ HH.text domain' ]
            ]
        Nothing -> [])
        <>
        (if required
        then []
        else
            [ divider
            , HH.span [ HP.class_ $ H.ClassName "profile-count" ] [ HH.text "optional" ]
            ])

fieldInput
    :: forall left
    .  Array { fieldKey :: String }
    -> Array { fieldKey :: String }
    ->  { key :: String
        , label :: String
        , type :: Int
        , required :: Boolean
        , domain :: Maybe String
        , options :: Maybe (Array { key :: String , option :: String })
        }
    ->  H.ComponentHTML Action ChildSlots (Async left)
fieldInput urlValueErrors missingErrors { key, type: 1, label, required, domain: Just domain } = let
    urlError = urlValueErrors # any (_.fieldKey >>> (_ == key))
    missingError = missingErrors # any (_.fieldKey >>> (_ == key))
    in
    HH.div_
    [ fieldLabel key label required (Just domain)
    , HH.input
        [ HP.id_ key
        , HP.class_ $ HH.ClassName "text-line-input"
        , HE.onValueInput $ Just <<< UrlValueInput key
        ]
    , HH.p
        [ HP.class_ $ inputErrorClass urlError ]
        [ HH.text $ "This doesn't look like a valid " <> label <> " (" <> domain <> ") address." ]
    , HH.p
        [ HP.class_ $ inputErrorClass missingError ]
        [ HH.text $ label <> " is required." ]
    ]
fieldInput _ _ { key, type: 2, label, required, options: Just options } =
    HH.div_
    [ fieldLabel key label required Nothing
    , singleSelectIndexed (SProxy :: SProxy "singleSelectField") key
        { options
        , selected: Nothing
        , labeler: _.option
        , comparer: \leftOption rightOption -> leftOption.key == rightOption.key
        }
    ]
fieldInput _ _ { key, type: 3, label, required, options: Just options } =
    HH.div_
    [ fieldLabel key label required Nothing
    , multiSelectIndexed (SProxy :: SProxy "multiSelectField") key
        { options: options <#> \option -> { option, selected: false }
        , labeler: _.option
        , comparer: \leftOption rightOption -> leftOption.key == rightOption.key
        }
    ]
fieldInput _ _ _ = HH.div_ []

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { summary, summaryError, urlValueErrors, missingErrors, otherError, game } = HH.form
    [ HP.class_ $ ClassName "single-form-wide", HE.onSubmit $ Just <<< Create ] $
    [ HH.h2_ [ HH.text $ "Create your " <> game.title <> " profile" ] ]
    <> (game.fields <#> fieldInput urlValueErrors missingErrors) <>
    [ HH.div_
        [ HH.label
            [ HP.for "summary" ]
            [ HH.text "Summary" ]
        , HH.textarea
            [ HP.id_ "summary"
            , HP.class_ $ HH.ClassName "text-input"
            , HE.onValueInput $ Just <<< SummaryInput
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass summaryError ]
            [ HH.text
                "The summary cannot be empty and cannot be more than 2000 characters long." ]
        ]
    , HH.button
        [ HP.class_ $ ClassName "primary-button"
        , HP.disabled $ summary == ""
        ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-user-plus button-icon" ] []
        , HH.text "Create profile"
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    ]

sendCreateRequest :: forall left. State -> String -> Async left (Maybe State)
sendCreateRequest state @ { summary, fieldValues, game } nickname = Async.unify do
    response <- Fetch.fetch
        ("/api/profiles/single/" <> game.handle <> "/" <> nickname)
        (  Fetch.method := POST
        <> Fetch.body := Json.writeJSON
            { summary
            , fieldValues: fieldValues # Array.filter
                \{ url, optionKey, optionKeys } ->
                    isJust url
                    || isJust optionKey
                    || (case optionKeys of
                        Just optionKeys' | not $ Array.null optionKeys' -> true
                        _ -> false)
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
                            , invalidUrl: \{ fieldKey, errors } ->
                                state' { urlValueErrors = Arary.cons { fieldKey } state'.urlValueErrors }
                            , missing: \{ fieldKey } ->
                                state' { missingErrors = Arary.cons { fieldKey } state'.missingErrors }
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
handleAction (UrlValueInput fieldKey url) = do
    state @ { fieldValues } <- H.get
    let newFieldValues = fieldValues <#> \value ->
            if value.fieldKey == fieldKey
                then
                    if null url
                    then value { url = Nothing }
                    else value { url = Just url }
                else value
    H.put $ state { fieldValues = newFieldValues }
handleAction (Create event) = do
    H.liftEffect $ preventDefault event
    state @ { game: { fields }, playerInfo: { nickname } } <- H.gets (_
        { summaryError   = false
        , otherError     = false
        , urlValueErrors = []
        , missingErrors  = []
        })
    singleSelectResults <-
        (H.queryAll (SProxy :: SProxy "singleSelectField")
        $ SingleSelect.Selected identity)
    let (singleSelectValues :: Array _) =
            singleSelectResults
            # Map.toUnfoldable
            <#> \(Tuple fieldKey option) ->
                { fieldKey
                , url: Nothing
                , optionKey: option <#> _.key
                , optionKeys: Nothing
                }
    multiSelectResults <-
        (H.queryAll (SProxy :: SProxy "multiSelectField")
        $ MultiSelect.Selected identity)
    let (multiSelectValues :: Array _) =
            multiSelectResults
            # Map.toUnfoldable
            <#> \(Tuple fieldKey options) ->
                { fieldKey
                , url: Nothing
                , optionKey: Nothing
                , optionKeys: Just $ options <#> _.key
                }
    let state' = state
            { fieldValues =
                state.fieldValues
                <> singleSelectValues
                <> multiSelectValues
            }
    newState <- H.lift $ sendCreateRequest state' nickname
    case newState of
        Nothing -> H.raise $ ProfileCreated state.game.handle
        Just newState' -> H.put newState'
    pure unit

component :: forall query left.
    H.Component HH.HTML query Input Message (Async left)
component = H.mkComponent
    { initialState: \{ game, playerInfo } ->
        { summary: ""
        , summaryError: false
        , fieldValues: game.fields <#> (\field ->
            { fieldKey: field.key
            , url: Nothing
            , optionKey: Nothing
            , optionKeys: Nothing
            })
        , urlValueErrors: []
        , missingErrors: []
        , otherError: false
        , game
        , playerInfo
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

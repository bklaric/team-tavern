module TeamTavern.Client.Game.Profiles (Query(..), Slot, gameProfiles) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (catMaybes, foldr, intercalate)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Foldable (find)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (ceil, floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Halogen (ClassName(..), defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Game.CreateProfile (createProfile)
import TeamTavern.Client.Game.CreateProfile as CreateProfile
import TeamTavern.Client.Game.GameHeader as GameHeader
import TeamTavern.Client.Profile.ProfileFilters (Filters)
import TeamTavern.Client.Script.Cookie (PlayerInfo, getPlayerInfo)
import TeamTavern.Client.Script.Cookie as Cookie
import TeamTavern.Server.Game.View.SendResponse as View
import TeamTavern.Server.Profile.ViewByGame.LoadProfiles (pageSize, pageSize')
import TeamTavern.Server.Profile.ViewByGame.SendResponse as ViewByGame
import Web.Event.Event (preventDefault)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type ProfilePage = Int

data Input = Input View.OkContent GameHeader.Tab

data Action
    = Init
    | Receive Input
    | ChangePage ProfilePage MouseEvent
    | ShowCreateProfileModal View.OkContent GameHeader.Tab PlayerInfo MouseEvent
    | HandleCreateProfileMessage GameHeader.Tab (Modal.Message CreateProfile.Message)

data State
    = Empty Input
    | Profiles
        View.OkContent
        GameHeader.Tab
        ProfilePage
        Filters
        ViewByGame.OkContent
        (Maybe Cookie.PlayerInfo)

data Query send = ApplyFilters Filters send

type Slot = H.Slot Query Void

type ChildSlots =
    ( players :: Anchor.Slot Int
    , createProfile :: CreateProfile.Slot Unit
    )

yearSeconds :: Number
yearSeconds = 60.0 * 60.0 * 24.0 * 365.0

monthSeconds :: Number
monthSeconds = 60.0 * 60.0 * 24.0 * 30.0

daySeconds :: Number
daySeconds = 60.0 * 60.0 * 24.0

hourSeconds :: Number
hourSeconds = 60.0 * 60.0

minuteSeconds :: Number
minuteSeconds = 60.0

lastUpdated :: Number -> String
lastUpdated updatedSeconds = let
    yearsAgo = floor(updatedSeconds / yearSeconds)
    monthsAgo = floor(updatedSeconds / monthSeconds)
    daysAgo = floor(updatedSeconds / daySeconds)
    hoursAgo = floor(updatedSeconds / hourSeconds)
    minutesAgo = floor(updatedSeconds / minuteSeconds)
    interval =
        if yearsAgo > 0 then Just { unit: "year", count: yearsAgo } else
        if monthsAgo > 0 then Just { unit: "month", count: monthsAgo } else
        if daysAgo > 0 then Just { unit: "day", count: daysAgo } else
        if hoursAgo > 0 then Just { unit: "hour", count: hoursAgo } else
        if minutesAgo > 0 then Just { unit: "minute", count: minutesAgo } else
        Nothing
    in
    case interval of
    Just { unit, count } -> show count <> " " <> unit <> (if count == 1 then "" else "s") <> " ago"
    Nothing -> "less than a minute ago"

totalPages :: Int -> Int
totalPages count = ceil (toNumber count / pageSize')

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Profiles game tab page _ response playerInfo') =
    HH.div [ HP.class_ $ HH.ClassName "card" ] $
    [ HH.span [ HP.class_ $ HH.ClassName "card-title" ] $ join
        [ pure $ HH.text
            case tab of
            GameHeader.Players -> "Player profiles"
            GameHeader.Teams -> "Team profiles"
        , pure divider
        , pure $ HH.span [ HP.class_ $ HH.ClassName "card-subtitle" ]
            [ HH.text $
                (if response.count == 0
                then "Showing 0"
                else
                    "Showing " <> show (1 + ((page - 1) * pageSize))
                    <> " - " <> show (min response.count (page * pageSize))
                    <> " out of " <> show response.count)
                <> case tab of
                    GameHeader.Players -> " players"
                    GameHeader.Teams -> " teams" ]
        , case Tuple tab playerInfo' of
            Tuple GameHeader.Players (Just playerInfo) | not game.hasPlayerProfile -> pure $ HH.button
                [ HP.class_ $ ClassName "primary-button"
                , HE.onClick $ Just <<< ShowCreateProfileModal game tab playerInfo
                ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-user-plus button-icon" ] []
                , HH.text "Create your profile"
                ]
            Tuple GameHeader.Teams (Just playerInfo) | not game.hasTeamProfile -> pure $ HH.button
                [ HP.class_ $ ClassName "primary-button"
                , HE.onClick $ Just <<< ShowCreateProfileModal game tab playerInfo
                ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-user-plus button-icon" ] []
                , HH.text "Create team profile"
                ]
            _ -> []
        ]
    , createProfile $ Just <<< HandleCreateProfileMessage tab
    ]
    <>
    if Array.null response.profiles
    then pure $
        HH.div [ HP.class_ $ ClassName "card-section" ]
        [ HH.p_ [ HH.text "No profiles satisfy specified filters." ] ]
    else
        (response.profiles # mapWithIndex \index { nickname, age, languages, hasMicrophone, summary, fieldValues, updated, updatedSeconds } ->
            HH.div [ HP.class_ $ ClassName "card-section" ] $
            [ HH.h3 [ HP.class_ $ ClassName "profile-title" ]
                [ navigationAnchorIndexed (SProxy :: SProxy "players") index
                    { path: "/players/" <> nickname, content: HH.text nickname }
                , divider
                , HH.span [ HP.class_ $ HH.ClassName "profile-updated" ]
                    [ HH.text $ "Updated " <> lastUpdated updatedSeconds ]
                ]
            ]
            <> Array.catMaybes
                [ age <#> \age' ->
                    HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                    [ HH.i [ HP.class_ $ HH.ClassName "fas fa-calendar-alt profile-field-icon" ] []
                    , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless profile-field-emphasize" ] [ HH.text $ show age' ]
                    , HH.text $ " years old"
                    ]
                , case languages of
                    [] -> Nothing
                    languages' -> Just $
                        HH.p [ HP.class_ $ HH.ClassName "profile-field" ] $
                        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-comments profile-field-icon" ] []
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Speaks " ]
                        ]
                        <>
                        ( foldr
                            (\language state ->
                                if not state.firstLanguage
                                then state { firstLanguage = true, languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ] ] }
                                else if not state.secondLanguage
                                then state { secondLanguage = true, languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ], HH.text " and " ] <> state.languagesSoFar }
                                else state { languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ], HH.text ", " ] <> state.languagesSoFar }
                            )
                            { firstLanguage: false, secondLanguage: false, languagesSoFar: [] }
                            languages'
                            # _.languagesSoFar
                        )
                , if hasMicrophone
                    then Just $
                        HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-microphone profile-field-icon" ] []
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless profile-field-emphasize" ] [ HH.text "Has microphone" ]
                        , HH.text $ " and is willing to communicate"
                        ]
                    else Nothing
                ]
            <> (Array.catMaybes $ game.fields <#> \field -> let
                fieldValue = fieldValues # find \ { fieldKey } -> field.key == fieldKey
                in
                case { tab, type: field.type, fieldValue } of
                { tab: GameHeader.Players, type: 1, fieldValue: Just { url: Just url' } } -> Just $
                    HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                    [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                    , HH.a [ HP.class_ $ HH.ClassName "profile-field-label", HP.href url' ] [ HH.text field.label ]
                    ]
                { tab: GameHeader.Players, type: 2, fieldValue: Just { optionKey: Just optionKey' } } -> let
                    fieldOption' = field.options >>= find (\{ key } -> key == optionKey')
                    in
                    fieldOption' <#> \{ option } ->
                        HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                        [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-label" ] [ HH.text $ field.label <> ": " ]
                        , HH.text option
                        ]
                { tab: GameHeader.Teams, type: 2, fieldValue: Just { optionKeys: Just optionKeys' } } -> let
                    fieldOptions' = field.options <#> Array.filter \{ key } -> Array.elem key optionKeys'
                    in
                    case fieldOptions' of
                    Just fieldOptions | not $ Array.null fieldOptions -> Just $
                        HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                        [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-label" ] [ HH.text $ field.label <> ": " ]
                        , HH.text $ intercalate ", " (fieldOptions <#> _.option)
                        ]
                    _ -> Nothing
                { tab: GameHeader.Players, type: 3, fieldValue: Just { optionKeys: Just optionKeys' } } -> let
                    fieldOptions' = field.options <#> Array.filter \{ key } -> Array.elem key optionKeys'
                    in
                    case fieldOptions' of
                    Just fieldOptions | not $ Array.null fieldOptions -> Just $
                        HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                        [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-label" ] [ HH.text $ field.label <> ": " ]
                        , HH.text $ intercalate ", " (fieldOptions <#> _.option)
                        ]
                    _ -> Nothing
                { tab: GameHeader.Teams, type: 3, fieldValue: Just { optionKeys: Just optionKeys' } } -> let
                    fieldOptions' = field.options <#> Array.filter \{ key } -> Array.elem key optionKeys'
                    in
                    case fieldOptions' of
                    Just fieldOptions | not $ Array.null fieldOptions -> Just $
                        HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                        [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-label" ] [ HH.text $ field.label <> ": " ]
                        , HH.text $ intercalate ", " (fieldOptions <#> _.option)
                        ]
                    _ -> Nothing
                _ ->  Nothing)
            <> (summary <#> \paragraph -> HH.p [ HP.class_ $ HH.ClassName "profile-summary" ] [ HH.text paragraph ])
        )
    <> (pure $
        HH.div [ HP.class_ $ HH.ClassName "card-footer" ]
            [ HH.div [ HP.class_$ HH.ClassName "pagination" ]
                [ HH.button
                    [ HP.class_ $ HH.ClassName "pagination-button"
                    , HP.disabled $ page == 1
                    , HE.onClick $ Just <<< ChangePage 1
                    ]
                    [ HH.text "First" ]
                , HH.button
                    [ HP.class_ $ HH.ClassName "pagination-button"
                    , HP.disabled $ page == 1
                    , HE.onClick $ Just <<< ChangePage (page - 1)
                    ]
                    [ HH.text "<" ]
                , HH.span [ HP.class_ $ HH.ClassName "pagination-page" ] [ HH.text $ show page <> "/" <> show (totalPages response.count) ]
                , HH.button
                    [ HP.class_ $ HH.ClassName "pagination-button"
                    , HP.disabled $ page == (totalPages response.count)
                    , HE.onClick $ Just <<< ChangePage (page + 1)
                    ]
                    [ HH.text ">" ]
                , HH.button
                    [ HP.class_ $ HH.ClassName "pagination-button"
                    , HP.disabled $ page == (totalPages response.count)
                    , HE.onClick $ Just <<< ChangePage (totalPages response.count)
                    ]
                    [ HH.text "Last" ]
                ]
            ]
        )

loadProfiles :: forall left. View.OkContent -> GameHeader.Tab -> ProfilePage -> Filters -> Async left State
loadProfiles game tab page filters = Async.unify do
    let empty = Empty (Input game tab)
    let tabPair =
            case tab of
            GameHeader.Players -> "ilk=1"
            GameHeader.Teams -> "ilk=2"
    let pagePair = "page=" <> show page
    let ageFromPair = filters.age.from <#> show <#> ("ageFrom=" <> _)
    let ageToPair = filters.age.to <#> show <#> ("ageTo=" <> _)
    let microphonePair = if filters.microphone then Just "microphone=true"else Nothing
    let languagePairs = filters.languages <#> (\language -> "languages=" <> language)
    let weekdayFromPair = filters.weekdayOnline.from <#> ("weekdayFrom=" <> _)
    let weekdayToPair = filters.weekdayOnline.to <#> ("weekdayTo=" <> _)
    let weekendFromPair = filters.weekendOnline.from <#> ("weekendFrom=" <> _)
    let weekendToPair = filters.weekendOnline.to <#> ("weekendTo=" <> _)
    let fieldPairs = filters.fields <#> (\field -> field.options <#> \option -> field.key <> "=" <> option.key) # join
    let allPairs = [tabPair, pagePair] <> languagePairs <> fieldPairs <> catMaybes
            ([ ageFromPair, ageToPair, microphonePair
            ,  weekdayFromPair, weekdayToPair, weekendFromPair, weekendToPair
            ])
    let filterQuery = "?" <> intercalate "&" allPairs
    response <- Fetch.fetch_
            ("/api/profiles/by-handle/" <> game.handle <> filterQuery)
        # lmap (const empty)
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const empty)
        _ -> Async.left empty
    playerInfo' <- H.liftEffect getPlayerInfo
    pure $ Profiles game tab page filters content playerInfo'

emptyFilters :: Filters
emptyFilters =
    { age: { from: Nothing, to: Nothing }
    , languages: []
    , microphone: false
    , weekdayOnline: { from: Nothing, to: Nothing }
    , weekendOnline: { from: Nothing, to: Nothing }
    , fields: []
    }

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Init = do
    state <- H.get
    case state of
        Empty (Input game tab) -> do
            state' <- H.lift $ loadProfiles game tab 1 emptyFilters
            H.put state'
        _ -> pure unit
handleAction (Receive (Input game tab)) = do
    state' <- H.lift $ loadProfiles game tab 1 emptyFilters
    H.put state'
    pure unit
handleAction (ChangePage page mouseEvent) = do
    state <- H.get
    case state of
        Profiles game tab _ filters _ playerInfo -> do
            newState <- H.lift $ loadProfiles game tab page filters
            H.put newState
        _ -> pure unit

handleAction (ShowCreateProfileModal game tab playerInfo event) = do
    H.liftEffect $ preventDefault $ toEvent event
    Modal.showWith { game, tab, playerInfo } (SProxy :: SProxy "createProfile")
handleAction (HandleCreateProfileMessage tab message) = do
    Modal.hide (SProxy :: SProxy "createProfile")
    case message of
        Modal.Inner (CreateProfile.ProfileCreated handle) ->
            window >>= location >>= reload # H.liftEffect
        _ -> pure unit

handleQuery
    :: forall output send left
    .  Query send
    -> H.HalogenM State Action ChildSlots output (Async left) (Maybe send)
handleQuery (ApplyFilters filters send) = do
    state <- H.get
    case state of
        Empty _ -> pure $ Just send
        Profiles game tab _ _ _ _ -> do
            state' <- H.lift $ loadProfiles game tab 1 filters
            H.put state'
            pure $ Just send

component :: forall output left.
    H.Component HH.HTML Query Input output (Async left)
component = mkComponent
    { initialState: Empty
    , render
    , eval: mkEval $ defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just $ Init
        , receive = Just <<< Receive
        }
    }

gameProfiles
    :: forall query children left
    .  View.OkContent
    -> GameHeader.Tab
    -> HH.ComponentHTML
        query (gameProfiles :: Slot Unit | children) (Async left)
gameProfiles game tab = HH.slot
    (SProxy :: SProxy "gameProfiles") unit component (Input game tab) absurd

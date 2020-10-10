module TeamTavern.Client.Pages.Profiles (Slot, profiles) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (intercalate)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Int (round)
import Data.Int as Int
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Effect (Effect, foreachE)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Pages.Profiles.CreatePlayerProfile (createPlayerProfile)
import TeamTavern.Client.Pages.Profiles.CreatePlayerProfile as CreatePlayerProfile
import TeamTavern.Client.Pages.Profiles.CreateTeamProfile (createTeamProfile)
import TeamTavern.Client.Pages.Profiles.CreateTeamProfile as CreateTeamProfile
import TeamTavern.Client.Pages.Profiles.GameHeader as GameHeader
import TeamTavern.Client.Pages.Profiles.PlayerProfiles (playerProfiles)
import TeamTavern.Client.Pages.Profiles.PlayerProfiles as PlayerProfiles
import TeamTavern.Client.Pages.Profiles.ProfileFilters (profileFilters)
import TeamTavern.Client.Pages.Profiles.ProfileFilters as ProfileFilters
import TeamTavern.Client.Pages.Profiles.TeamProfiles (teamProfiles)
import TeamTavern.Client.Pages.Profiles.TeamProfiles as TeamProfiles
import TeamTavern.Client.Script.Cookie (PlayerInfo, getPlayerInfo)
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Script.Url as Url
import TeamTavern.Server.Game.View.SendResponse as ViewGame
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame.SendResponse as ViewGamePlayers
import TeamTavern.Server.Profile.ViewTeamProfilesByGame.SendResponse as ViewGameTeams
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode as ParentNode
import Web.HTML (window)
import Web.HTML as Html
import Web.HTML.HTMLDocument as HtmlDocument
import Web.HTML.HTMLElement as HtmlElement
import Web.HTML.Location (reload)
import Web.HTML.Location as Location
import Web.HTML.Window (location)
import Web.HTML.Window as Window

type ShowCreateModal = Boolean

type Timezone = String

data Tab
    = Players PlayerProfiles.Input ShowCreateModal
    | Teams TeamProfiles.Input ShowCreateModal Timezone

toHeaderTab :: Tab -> GameHeader.Tab
toHeaderTab (Players _ _) = GameHeader.Players
toHeaderTab (Teams _ _ _) = GameHeader.Teams

data Input = Input GameHeader.Handle GameHeader.Tab

data Action
    = Init
    | Receive Input
    | ApplyFilters ProfileFilters.Filters
    | ShowCreateProfileModal
    | HideCreateProfileModal
    | ReloadPage
    | ChangePage Int

data State
    = Empty Input
    | Game ViewGame.OkContent (Maybe PlayerInfo) ProfileFilters.Filters Tab
    | NotFound
    | Error

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( gameHeader :: H.Slot (Const Void) Void Unit
    , playerProfiles :: PlayerProfiles.Slot
    , teamProfiles :: TeamProfiles.Slot
    , profileFilters :: ProfileFilters.Slot
    , createPlayerProfile :: CreatePlayerProfile.Slot
    , createTeamProfile :: CreateTeamProfile.Slot
    )

filterableFields
    :: Array
        { ilk :: Int
        , key :: String
        , label :: String
        , icon :: String
        , required :: Boolean
        , domain :: Maybe String
        , options :: Maybe (Array
            { key :: String
            , label :: String
            })
        }
    -> Array ProfileFilters.Field
filterableFields fields = fields # Array.mapMaybe
    case _ of
    { key, label, icon, ilk, options: Just options }
        | ilk == 2 || ilk == 3 -> Just { key, label, icon, options }
    _ -> Nothing

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Game game' player filters tab) = let
    gameHeader =
        HH.slot (SProxy :: SProxy "gameHeader") unit GameHeader.component
        (GameHeader.Input game'.handle game'.title $ toHeaderTab tab) absurd
    in
    HH.div_ $
    [ gameHeader
    , HH.div [ HP.class_ $ HH.ClassName "profiles-container" ]
        [ profileFilters { fields: filterableFields game'.fields, filters }
            (\(ProfileFilters.Apply filters') -> Just $ ApplyFilters filters')
        , case tab of
            Players input _ ->
                playerProfiles
                input
                case _ of
                PlayerProfiles.CreateProfile -> Just ShowCreateProfileModal
                PlayerProfiles.ChangePage page -> Just $ ChangePage page
            Teams input _ _ ->
                teamProfiles
                input
                case _ of
                TeamProfiles.CreateProfile -> Just ShowCreateProfileModal
                TeamProfiles.ChangePage page -> Just $ ChangePage page
        ]
    ]
    <>
    case player, tab of
    Just player', (Players _ true) -> Array.singleton $
        createPlayerProfile
        { game: game', player: player' }
        case _ of
        Modal.CloseClicked -> Just HideCreateProfileModal
        Modal.OutputRaised (CreatePlayerProfile.CloseClicked) -> Just HideCreateProfileModal
        Modal.OutputRaised (CreatePlayerProfile.ProfileCreated) -> Just ReloadPage
    Just player', (Teams _ true timezone) -> Array.singleton $
        createTeamProfile
        { game: game', player: player', timezone }
        case _ of
        Modal.CloseClicked -> Just HideCreateProfileModal
        Modal.OutputRaised (CreateTeamProfile.CloseClicked) -> Just HideCreateProfileModal
        Modal.OutputRaised (CreateTeamProfile.ProfileCreated) -> Just ReloadPage
    _, _ -> []
render NotFound = HH.p_ [ HH.text "Game could not be found." ]
render Error = HH.p_ [ HH.text
    "There has been an error loading the game. Please try again later." ]

loadGame :: forall left. String -> Async left (Maybe ViewGame.OkContent)
loadGame handle = Async.unify do
    response <-
        Fetch.fetch_ ("/api/games/by-handle/" <> handle)
        # lmap (const Nothing)
    content <-
        case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Nothing)
        404 -> Async.left Nothing
        _ -> Async.left Nothing
    pure $ Just content

loadPlayerProfiles :: forall left.
    String -> Int -> ProfileFilters.Filters -> Async left (Maybe ViewGamePlayers.OkContent)
loadPlayerProfiles handle page filters = Async.unify do
    timezone <- getClientTimezone
    let nothingIfNull string = if String.null string then Nothing else Just string
    let pagePair = "page=" <> show page
        timezonePair = "timezone=" <> timezone
        ageFromPair = filters.ageFrom <#> show <#> ("ageFrom=" <> _)
        ageToPair = filters.ageTo <#> show <#> ("ageTo=" <> _)
        languagePairs = filters.languages <#> ("languages=" <> _)
        countryPairs = filters.countries <#> (\country -> "countries=" <> country)
        microphonePair = if filters.microphone then Just "microphone=true" else Nothing
        weekdayFromPair = filters.weekdayFrom <#> ("weekdayFrom=" <> _)
        weekdayToPair = filters.weekdayTo <#> ("weekdayTo=" <> _)
        weekendFromPair = filters.weekendFrom <#> ("weekendFrom=" <> _)
        weekendToPair = filters.weekendTo <#> ("weekendTo=" <> _)
        fieldPairs =
            filters.fields
            <#> (\{ fieldKey, optionKey } -> fieldKey <> "=" <> optionKey)
        newOrReturningPair = if filters.newOrReturning then Just "newOrReturning=true" else Nothing
        allPairs = [pagePair, timezonePair]
            <> languagePairs <> countryPairs <> fieldPairs <> Array.catMaybes
            [ ageFromPair, ageToPair, microphonePair, newOrReturningPair
            , weekdayFromPair, weekdayToPair, weekendFromPair, weekendToPair
            ]
        filterQuery = "?" <> intercalate "&" allPairs
    response <-
        Fetch.fetch_ ("/api/profiles/by-handle/" <> handle <> "/players" <> filterQuery)
        # lmap (const Nothing)
    content <-
        case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Nothing)
        _ -> Async.left Nothing
    pure content

loadTeamProfiles :: forall left.
    String -> Int -> ProfileFilters.Filters -> Async left (Maybe ViewGameTeams.OkContent)
loadTeamProfiles handle page filters = Async.unify do
    timezone <- getClientTimezone
    let nothingIfNull string = if String.null string then Nothing else Just string
    let pagePair = "page=" <> show page
        timezonePair = "timezone=" <> timezone
        ageFromPair = filters.ageFrom <#> show <#> ("ageFrom=" <> _)
        ageToPair = filters.ageTo <#> show <#> ("ageTo=" <> _)
        languagePairs = filters.languages <#> ("languages=" <> _)
        countryPairs = filters.countries <#> (\country -> "countries=" <> country)
        microphonePair = if filters.microphone then Just "microphone=true" else Nothing
        weekdayFromPair = filters.weekdayFrom <#> ("weekdayFrom=" <> _)
        weekdayToPair = filters.weekdayTo <#> ("weekdayTo=" <> _)
        weekendFromPair = filters.weekendFrom <#> ("weekendFrom=" <> _)
        weekendToPair = filters.weekendTo <#> ("weekendTo=" <> _)
        fieldPairs =
            filters.fields
            <#> (\{ fieldKey, optionKey } -> fieldKey <> "=" <> optionKey)
        newOrReturningPair = if filters.newOrReturning then Just "newOrReturning=true" else Nothing
        allPairs = [pagePair, timezonePair]
            <> languagePairs <> countryPairs <> fieldPairs <> Array.catMaybes
            [ ageFromPair, ageToPair, microphonePair, newOrReturningPair
            , weekdayFromPair, weekdayToPair, weekendFromPair, weekendToPair
            ]
        filterQuery = "?" <> intercalate "&" allPairs
    response <-
        Fetch.fetch_ ("/api/profiles/by-handle/" <> handle <> "/teams" <> filterQuery)
        # lmap (const Nothing)
    content <-
        case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Nothing)
        _ -> Async.left Nothing
    pure content

loadTab :: forall output left.
    String -> GameHeader.Tab -> H.HalogenM State Action ChildSlots output (Async left) Unit
loadTab handle GameHeader.Players = do
    game <- H.lift $ loadGame handle
    case game of
        Just game' -> do
            { page, filters } <- H.liftEffect $ readQueryParams game'.fields
            playerProfiles' <- H.lift $ loadPlayerProfiles handle page filters
            case playerProfiles' of
                Just playerProfiles'' -> do
                    player <- getPlayerInfo
                    H.put $ Game game' player filters $ Players
                        { profiles: playerProfiles''.profiles
                        , profileCount: playerProfiles''.count
                        , page
                        , showCreateProfile:
                            isJust player && not game'.hasPlayerProfile
                        , playerInfo: player
                        }
                        false
                    H.liftEffect $ setMetaTags game'.title GameHeader.Players
                Nothing -> do
                    H.put Error
                    H.liftEffect $ setMetaTags handle GameHeader.Players
        Nothing -> do
            H.put Error
            H.liftEffect $ setMetaTags handle GameHeader.Players
loadTab handle GameHeader.Teams = do
    game <- H.lift $ loadGame handle
    case game of
        Just game' -> do
            { page, filters } <- H.liftEffect $ readQueryParams game'.fields
            teamProfiles' <- H.lift $ loadTeamProfiles handle page filters
            case teamProfiles' of
                Just teamProfiles'' -> do
                    player <- getPlayerInfo
                    timezone <- getClientTimezone
                    H.put $ Game game' player filters $ Teams
                        { profiles: teamProfiles''.profiles
                        , profileCount: teamProfiles''.count
                        , page
                        , showCreateProfile:
                            isJust player && not game'.hasPlayerProfile
                        , playerInfo: player
                        }
                        false
                        timezone
                    H.liftEffect $ setMetaTags game'.title GameHeader.Players
                Nothing -> do
                    H.put Error
                    H.liftEffect $ setMetaTags handle GameHeader.Players
        Nothing -> do
            H.put Error
            H.liftEffect $ setMetaTags handle GameHeader.Players

setMetaTags :: String -> GameHeader.Tab -> Effect Unit
setMetaTags handleOrTitle tab =
    case tab of
    GameHeader.Players -> do
        setMetaTitle $ handleOrTitle <> " players - Looking for team | TeamTavern"
        setMetaDescription $ "Browse and filter " <> handleOrTitle <> " players looking for a team."
        setMetaUrl
    GameHeader.Teams -> do
        setMetaTitle $ handleOrTitle <> " teams - Looking for players | TeamTavern"
        setMetaDescription $ "Browse and filter " <> handleOrTitle <> " teams looking for players."
        setMetaUrl

scrollProfilesIntoView :: forall monad. Bind monad => MonadEffect monad => monad Unit
scrollProfilesIntoView = do
    profileCard <- Html.window >>= Window.document <#> HtmlDocument.toParentNode
        >>= ParentNode.querySelector (QuerySelector "#profiles-card")
        <#> (\element -> element >>= HtmlElement.fromElement)
        # H.liftEffect
    case profileCard of
        Just element -> do
            offsetTop <- H.liftEffect $ round <$> HtmlElement.offsetTop element
            Html.window >>= Window.scroll 0 (offsetTop - 41 - 21) # H.liftEffect
        Nothing -> pure unit

readQueryParams
    :: forall fields
    .  Array { key :: String | fields }
    -> Effect { filters :: ProfileFilters.Filters, page :: Int }
readQueryParams fields' = do
    searchParams <- Html.window >>= Window.location >>= Location.href
        >>= Url.url >>= Url.searchParams
    page <- Url.get "page" searchParams <#> maybe 1 (Int.fromString >>> maybe 1 identity)
    ageFrom <- Url.get "age-from" searchParams <#> (\ageFrom -> ageFrom >>= Int.fromString)
    ageTo <- Url.get "age-to" searchParams <#> (\ageTo -> ageTo >>= Int.fromString)
    countries <- Url.getAll "location" searchParams
    languages <- Url.getAll "language" searchParams
    microphone <- Url.get "microphone" searchParams <#> isJust
    weekdayFrom <- Url.get "weekday-from" searchParams
    weekdayTo <- Url.get "weekday-to" searchParams
    weekendFrom <- Url.get "weekend-from" searchParams
    weekendTo <- Url.get "weekend-to" searchParams
    (fields :: Array { fieldKey :: String, optionKey :: String }) <-
        fields'
        # traverse (\field ->
            Url.getAll field.key searchParams
            <#> \values -> values <#> { fieldKey: field.key, optionKey: _ })
        <#> join
    newOrReturning <- Url.get "new-or-returning" searchParams <#> isJust
    pure
        { page
        , filters:
            { ageFrom
            , ageTo
            , countries
            , languages
            , microphone
            , weekdayFrom
            , weekdayTo
            , weekendFrom
            , weekendTo
            , fields
            , newOrReturning
            }
        }

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Init = do
    state <- H.get
    case state of
        Empty (Input handle tab) -> handleAction $ Receive $ Input handle tab
        _ -> pure unit
handleAction (Receive (Input handle tab)) = do
    state <- H.get
    case state, tab of
        Game game player _ _, GameHeader.Players | game.handle == handle -> do
            { page, filters } <- H.liftEffect $ readQueryParams game.fields
            playerProfiles <- H.lift $ loadPlayerProfiles handle page filters
            case playerProfiles of
                Just playerProfiles' -> do
                    H.put $ Game game player filters $ Players
                        { profiles: playerProfiles'.profiles
                        , profileCount: playerProfiles'.count
                        , page: page
                        , showCreateProfile:
                            isJust player && not game.hasPlayerProfile
                        , playerInfo: player
                        }
                        false
                    H.liftEffect $ setMetaTags game.title tab
                Nothing -> pure unit
        Game game player _ _, GameHeader.Teams | game.handle == handle -> do
            { page, filters } <- H.liftEffect $ readQueryParams game.fields
            teamProfiles <- H.lift $ loadTeamProfiles handle page filters
            case teamProfiles of
                Just teamProfiles' -> do
                    timezone <- getClientTimezone
                    H.put $ Game game player filters $ Teams
                        { profiles: teamProfiles'.profiles
                        , profileCount: teamProfiles'.count
                        , page: page
                        , showCreateProfile:
                            isJust player && not game.hasTeamProfile
                        , playerInfo: player
                        }
                        false
                        timezone
                    H.liftEffect $ setMetaTags game.title tab
                Nothing -> pure unit
        _, _ -> loadTab handle tab
handleAction (ApplyFilters filters) = do
    scrollProfilesIntoView

    H.liftEffect do
        url <- Html.window >>= Window.location >>= Location.href >>= Url.url
        searchParams <- Url.searchParams url

        keys <- Url.keys searchParams
        foreachE keys \key -> Url.delete key searchParams

        Url.set "page" "1" searchParams
        case filters.ageFrom of
            Nothing -> pure unit
            Just ageFrom -> Url.set "age-from" (show ageFrom) searchParams
        case filters.ageTo of
            Nothing -> pure unit
            Just ageTo -> Url.set "age-to" (show ageTo) searchParams
        foreachE filters.countries \location ->
            Url.append "location" location searchParams
        foreachE filters.languages \language ->
            Url.append "language" language searchParams
        if filters.microphone
            then Url.set "microphone" "true" searchParams
            else pure unit
        case filters.weekdayFrom of
            Nothing -> pure unit
            Just weekdayFrom -> Url.set "weekday-from" weekdayFrom searchParams
        case filters.weekdayTo of
            Nothing -> pure unit
            Just weekdayTo -> Url.set "weekday-to" weekdayTo searchParams
        case filters.weekendFrom of
            Nothing -> pure unit
            Just weekendFrom -> Url.set "weekend-from" weekendFrom searchParams
        case filters.weekendTo of
            Nothing -> pure unit
            Just weekendTo -> Url.set "weekend-to" weekendTo searchParams
        foreachE filters.fields \{fieldKey, optionKey } ->
            Url.append fieldKey optionKey searchParams
        if filters.newOrReturning
            then Url.set "new-or-returning" "true" searchParams
            else pure unit

        href <- Url.href url
        navigate_ href

handleAction ShowCreateProfileModal =
    H.modify_
    case _ of
    Game game' player filters (Players input _) ->
        Game game' player filters $ Players input true
    Game game' player filters (Teams input _ timezone) ->
        Game game' player filters $ Teams input true timezone
    other -> other
handleAction HideCreateProfileModal =
    H.modify_
    case _ of
    Game game' player filters (Players input _) ->
        Game game' player filters $ Players input false
    Game game' player filters (Teams input _ timezone) ->
        Game game' player filters $ Teams input false timezone
    other -> other
handleAction ReloadPage =
    window >>= location >>= reload # H.liftEffect
handleAction (ChangePage page) = do
    scrollProfilesIntoView
    H.liftEffect do
        url <- Html.window >>= Window.location >>= Location.href >>= Url.url
        searchParams <- Url.searchParams url
        Url.set "page" (show page) searchParams
        href <- Url.href url
        navigate_ href

component :: forall query output left.
    H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        , receive = Just <<< Receive
        }
    }

profiles :: forall query children left.
    String -> GameHeader.Tab -> HH.ComponentHTML query (profiles :: Slot Unit | children) (Async left)
profiles handle tab =
    HH.slot (SProxy :: SProxy "profiles") unit component (Input handle tab) absurd

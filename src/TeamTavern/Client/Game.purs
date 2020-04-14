module TeamTavern.Client.Game (Slot, game) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Control.Parallel (parallel, sequential)
import Data.Array (intercalate)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Maybe (Maybe(..), isJust)
import Data.String as String
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Halogen as H
import Halogen.HTML as HH
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.ModalDeclarative as Modal
import TeamTavern.Client.Game.CreatePlayerProfile (createPlayerProfile)
import TeamTavern.Client.Game.CreatePlayerProfile as CreatePlayerProfile
import TeamTavern.Client.Game.CreateTeamProfile (createTeamProfile)
import TeamTavern.Client.Game.CreateTeamProfile as CreateTeamProfile
import TeamTavern.Client.Game.GameHeader as GameHeader
import TeamTavern.Client.Game.PlayerProfiles (Input, Message(..), Slot) as PlayerProfiles
import TeamTavern.Client.Game.PlayerProfiles (playerProfiles)
import TeamTavern.Client.Game.TeamProfiles (teamProfiles)
import TeamTavern.Client.Game.TeamProfiles as TeamProfiles
import TeamTavern.Client.Profile.ProfileFilters (Filters, filterProfiles)
import TeamTavern.Client.Profile.ProfileFilters as ProfileFilters
import TeamTavern.Client.Script.Cookie (PlayerInfo, getPlayerInfo)
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Server.Game.View.SendResponse as ViewGame
import TeamTavern.Server.Profile.ViewGamePlayers.SendResponse (OkContent) as ViewGamePlayers
import TeamTavern.Server.Profile.ViewGameTeams.SendResponse as ViewGameTeams
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)

type ShowCreateModal = Boolean

data Tab
    = Players PlayerProfiles.Input ShowCreateModal
    | Teams TeamProfiles.Input ShowCreateModal String

toHeaderTab :: Tab -> GameHeader.Tab
toHeaderTab (Players _ _) = GameHeader.Players
toHeaderTab (Teams _ _ _) = GameHeader.Teams

data Input = Input GameHeader.Handle GameHeader.Tab

data Action
    = Init
    | Receive Input
    | ApplyFilters Filters
    | ShowCreateProfileModal
    | HideCreateProfileModal
    | ReloadPage
    | ChangePage Int

data State
    = Empty Input
    | Game ViewGame.OkContent (Maybe PlayerInfo) Tab
    | NotFound
    | Error

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( gameHeader :: H.Slot (Const Void) Void Unit
    , playerProfiles :: PlayerProfiles.Slot
    , teamProfiles :: TeamProfiles.Slot
    , filterProfiles :: ProfileFilters.Slot
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
render (Game game' player tab) = let
    gameHeader =
        HH.slot (SProxy :: SProxy "gameHeader") unit GameHeader.component
        (GameHeader.Input game'.handle game'.title $ toHeaderTab tab) absurd
    in
    HH.div_ $
    [ gameHeader
    , filterProfiles (filterableFields game'.fields)
        (\(ProfileFilters.Apply filters) -> Just $ ApplyFilters filters)
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
    <>
    case player, tab of
    Just player', (Players _ true) -> Array.singleton $
        createPlayerProfile
        { game: game', player: player' }
        case _ of
        Modal.BackgroundClicked -> Just HideCreateProfileModal
        Modal.OutputRaised (CreatePlayerProfile.CloseClicked) -> Just HideCreateProfileModal
        Modal.OutputRaised (CreatePlayerProfile.ProfileCreated) -> Just ReloadPage
    Just player', (Teams _ true timezone) -> Array.singleton $
        createTeamProfile
        { game: game', player: player', timezone }
        case _ of
        Modal.BackgroundClicked -> Just HideCreateProfileModal
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
    timezone <- H.liftEffect getClientTimezone
    let nothingIfNull string = if String.null string then Nothing else Just string
    let pagePair = "page=" <> show page
        timezonePair = "timezone=" <> timezone
        ageFromPair = filters.ageFrom <#> show <#> ("ageFrom=" <> _)
        ageToPair = filters.ageTo <#> show <#> ("ageTo=" <> _)
        languagePairs = filters.languages <#> ("languages=" <> _)
    --     countryPairs = filters.countries <#> (\country -> "countries=" <> country)
        microphonePair = if filters.microphone then Just "microphone=true" else Nothing
        weekdayFromPair = filters.weekdayFrom <#> ("weekdayFrom=" <> _)
        weekdayToPair = filters.weekdayTo <#> ("weekdayTo=" <> _)
        weekendFromPair = filters.weekendFrom <#> ("weekendFrom=" <> _)
        weekendToPair = filters.weekendTo <#> ("weekendTo=" <> _)
        fieldPairs =
            filters.fields
            <#> (\{ fieldKey, optionKey } -> fieldKey <> "=" <> optionKey)
        allPairs = [pagePair, timezonePair]
            <> languagePairs <> fieldPairs <> Array.catMaybes
            [ ageFromPair, ageToPair, microphonePair
            ,  weekdayFromPair, weekdayToPair, weekendFromPair, weekendToPair
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
    timezone <- H.liftEffect getClientTimezone
    let nothingIfNull string = if String.null string then Nothing else Just string
    let pagePair = "page=" <> show page
        timezonePair = "timezone=" <> timezone
        ageFromPair = filters.ageFrom <#> show <#> ("ageFrom=" <> _)
        ageToPair = filters.ageTo <#> show <#> ("ageTo=" <> _)
        languagePairs = filters.languages <#> ("languages=" <> _)
    --     countryPairs = filters.countries <#> (\country -> "countries=" <> country)
        microphonePair = if filters.microphone then Just "microphone=true" else Nothing
        weekdayFromPair = filters.weekdayFrom <#> ("weekdayFrom=" <> _)
        weekdayToPair = filters.weekdayTo <#> ("weekdayTo=" <> _)
        weekendFromPair = filters.weekendFrom <#> ("weekendFrom=" <> _)
        weekendToPair = filters.weekendTo <#> ("weekendTo=" <> _)
        fieldPairs =
            filters.fields
            <#> (\{ fieldKey, optionKey } -> fieldKey <> "=" <> optionKey)
        allPairs = [pagePair, timezonePair]
            <> languagePairs <> fieldPairs <> Array.catMaybes
            [ ageFromPair, ageToPair, microphonePair
            ,  weekdayFromPair, weekdayToPair, weekendFromPair, weekendToPair
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
    { gameContent, profilesContent } <-
        H.lift
        $ sequential
        $ { gameContent: _, profilesContent: _}
        <$> parallel (loadGame handle)
        <*> parallel (loadPlayerProfiles handle 1 ProfileFilters.emptyFilters)
    case gameContent, profilesContent of
        Just gameContent', Just profilesContent' -> do
            player <- H.liftEffect getPlayerInfo
            H.put $ Game gameContent' player $ Players
                { profiles: profilesContent'.profiles
                , profileCount: profilesContent'.count
                , page: 1
                , showCreateProfile:
                    isJust player && not gameContent'.hasPlayerProfile
                }
                false
            H.liftEffect $ setMetaTags gameContent'.title GameHeader.Players
        _, _ -> do
            H.put Error
            H.liftEffect $ setMetaTags handle GameHeader.Players
loadTab handle GameHeader.Teams = do
    { gameContent, profilesContent } <-
        H.lift
        $ sequential
        $ { gameContent: _, profilesContent: _}
        <$> parallel (loadGame handle)
        <*> parallel (loadTeamProfiles handle 1 ProfileFilters.emptyFilters)
    case gameContent, profilesContent of
        Just gameContent', Just profilesContent' -> do
            player <- H.liftEffect getPlayerInfo
            timezone <- H.liftEffect getClientTimezone
            H.put $ Game gameContent' player $ Teams
                { profiles: profilesContent'.profiles
                , profileCount: profilesContent'.count
                , page: 1
                , showCreateProfile:
                    isJust player && not gameContent'.hasTeamProfile
                }
                false
                timezone
            H.liftEffect $ setMetaTags gameContent'.title GameHeader.Players
        _, _ -> do
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

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Init = do
    state <- H.get
    case state of
        Empty (Input handle tab) -> loadTab handle tab
        _ -> pure unit
handleAction (Receive (Input _ inputTab)) = do
    state <- H.get
    case state, inputTab of
        Game content player _, GameHeader.Players -> do
            profilesContent <- H.lift $
                loadPlayerProfiles content.handle 1 ProfileFilters.emptyFilters
            case profilesContent of
                Just profilesContent' -> do
                    H.put $ Game content player $ Players
                        { profiles: profilesContent'.profiles
                        , profileCount: profilesContent'.count
                        , page: 1
                        , showCreateProfile:
                            isJust player && not content.hasPlayerProfile
                        }
                        false
                    H.liftEffect $ setMetaTags content.title inputTab
                Nothing -> pure unit
        Game content player _, GameHeader.Teams -> do
            profilesContent <- H.lift $
                loadTeamProfiles content.handle 1 ProfileFilters.emptyFilters
            case profilesContent of
                Just profilesContent' -> do
                    timezone <- H.liftEffect getClientTimezone
                    H.put $ Game content player $ Teams
                        { profiles: profilesContent'.profiles
                        , profileCount: profilesContent'.count
                        , page: 1
                        , showCreateProfile:
                            isJust player && not content.hasTeamProfile
                        }
                        false
                        timezone
                    H.liftEffect $ setMetaTags content.title inputTab
                Nothing -> pure unit
        _, _ -> pure unit
handleAction (ApplyFilters filters) = do
    state <- H.get
    case state of
        Game game' player (Players input _) -> do
            profilesContent <- H.lift $
                loadPlayerProfiles game'.handle 1 filters
            case profilesContent of
                Just profilesContent' ->
                    H.put $ Game game' player $ Players
                        { profiles: profilesContent'.profiles
                        , profileCount: profilesContent'.count
                        , page: 1
                        , showCreateProfile: input.showCreateProfile
                        }
                        false
                Nothing -> pure unit
        Game game' player (Teams input _ timezone) -> do
            profilesContent <- H.lift $
                loadTeamProfiles game'.handle 1 filters
            case profilesContent of
                Just profilesContent' ->
                    H.put $ Game game' player $ Teams
                        { profiles: profilesContent'.profiles
                        , profileCount: profilesContent'.count
                        , page: 1
                        , showCreateProfile: input.showCreateProfile
                        }
                        false
                        timezone
                Nothing -> pure unit
        _ -> pure unit
handleAction ShowCreateProfileModal =
    H.modify_
    case _ of
    Game game' player (Players input _) ->
        Game game' player $ Players input true
    Game game' player (Teams input _ timezone) ->
        Game game' player $ Teams input true timezone
    other -> other
handleAction HideCreateProfileModal =
    H.modify_
    case _ of
    Game game' player (Players input _) ->
        Game game' player $ Players input false
    Game game' player (Teams input _ timezone) ->
        Game game' player $ Teams input false timezone
    other -> other
handleAction ReloadPage =
    window >>= location >>= reload # H.liftEffect
handleAction (ChangePage page) =
    pure unit

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

game :: forall query children left.
    String -> GameHeader.Tab -> HH.ComponentHTML query (game :: Slot Unit | children) (Async left)
game handle tab =
    HH.slot (SProxy :: SProxy "game") unit component (Input handle tab) absurd

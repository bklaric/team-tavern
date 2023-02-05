module TeamTavern.Client.Pages.Profiles (Input, profiles) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Control.Bind (bindFlipped)
import Data.Array (foldl, intercalate)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Int (round)
import Data.Int as Int
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.MultiMap as MultiMap
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect, foreachE)
import Effect.Class (class MonadEffect)
import Effect.Timer (setTimeout)
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Boarding.PlayerOrTeamInput as Boarding
import TeamTavern.Client.Components.Content (contentColumns)
import TeamTavern.Client.Components.Team.ProfileInputGroup (FieldValues)
import TeamTavern.Client.Pages.Preboarding as Preboarding
import TeamTavern.Client.Pages.Profile.Filters (Filters)
import TeamTavern.Client.Pages.Profiles.GameHeader as GameHeader
import TeamTavern.Client.Pages.Profiles.PlayerProfiles (playerProfiles)
import TeamTavern.Client.Pages.Profiles.PlayerProfiles as PlayerProfiles
import TeamTavern.Client.Pages.Profiles.ProfileFilters (profileFilters)
import TeamTavern.Client.Pages.Profiles.ProfileFilters as ProfileFilters
import TeamTavern.Client.Pages.Profiles.TeamProfiles (teamProfiles)
import TeamTavern.Client.Pages.Profiles.TeamProfiles as TeamProfiles
import TeamTavern.Client.Script.Analytics (track, track_)
import TeamTavern.Client.Script.Cookie (PlayerInfo, getPlayerInfo)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Navigate (navigate, navigate_)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Script.Url as Url
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.ArticledNoun (indefiniteNoun)
import TeamTavern.Routes.Game.ViewGame as ViewGame
import TeamTavern.Routes.Profile.ViewPlayerProfilesByGame as ViewPlayerProfilesByGame
import TeamTavern.Routes.Profile.ViewTeamProfilesByGame as ViewTeamProfilesByGame
import TeamTavern.Routes.Shared.Organization as Organization
import TeamTavern.Routes.Shared.Platform as Platform
import TeamTavern.Routes.Shared.Size as Size
import Type.Proxy (Proxy(..))
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
import Yoga.JSON.Async as Json

data Tab
    = Players PlayerProfiles.Input
    | Teams TeamProfiles.Input

toHeaderProfileTab :: Tab -> GameHeader.ProfileTab
toHeaderProfileTab (Players _) = GameHeader.Players
toHeaderProfileTab (Teams _) = GameHeader.Teams

type Input = { game :: ViewGame.OkContent, tab :: GameHeader.ProfileTab }

data Action
    = Init
    | Receive Input
    | ApplyFilters Filters
    | ReloadPage
    | ChangePage Int
    | OpenPlayerPreboarding
    | OpenTeamPreboarding

data State
    = Empty Input
    | Game ViewGame.OkContent (Maybe PlayerInfo) Filters Tab
    | Error

type ChildSlots =
    ( gameHeader :: Slot___
    , playerProfiles :: PlayerProfiles.Slot
    , teamProfiles :: TeamProfiles.Slot
    , profileFilters :: ProfileFilters.Slot
    )

filterableFields
    :: Array
        { ilk :: Int
        , key :: String
        , label :: String
        , icon :: String
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

render :: ∀ left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Game game _ filters tab) =
    HH.div_ $
    [ contentColumns
        [ profileFilters
            { platforms: game.platforms
            , fields: filterableFields game.fields
            , filters
            , tab: toHeaderProfileTab tab
            , handle: game.handle
            }
            (\(ProfileFilters.Apply filters') -> ApplyFilters filters')
        , case tab of
            Players input ->
                playerProfiles
                input
                case _ of
                PlayerProfiles.PageChanged page -> ChangePage page
                PlayerProfiles.PreboardingClicked -> OpenPlayerPreboarding
            Teams input ->
                teamProfiles
                input
                case _ of
                TeamProfiles.PageChanged page -> ChangePage page
                TeamProfiles.PreboardingClicked -> OpenTeamPreboarding
        ]
    ]
render Error = HH.p_ [ HH.text "There has been an error loading profiles. Please try again later." ]

loadPlayerProfiles :: ∀ left.
    String -> Int -> Filters -> Async left (Maybe ViewPlayerProfilesByGame.OkContent)
loadPlayerProfiles handle page filters = Async.unify do
    timezone <- getClientTimezone
    let pagePair = "page=" <> show page
        timezonePair = "timezone=" <> timezone
        ageFromPair = filters.ageFrom <#> show <#> ("ageFrom=" <> _)
        ageToPair = filters.ageTo <#> show <#> ("ageTo=" <> _)
        languagePairs = filters.languages <#> ("language=" <> _)
        locationPairs = filters.locations <#> (\location -> "location=" <> location)
        microphonePair = if filters.microphone then Just "microphone=true" else Nothing
        weekdayFromPair = filters.weekdayFrom <#> ("weekdayFrom=" <> _)
        weekdayToPair = filters.weekdayTo <#> ("weekdayTo=" <> _)
        weekendFromPair = filters.weekendFrom <#> ("weekendFrom=" <> _)
        weekendToPair = filters.weekendTo <#> ("weekendTo=" <> _)
        platformPairs = filters.platforms <#> Platform.toString <#> ("platform=" <> _)
        fieldPairs = filters.fieldValues # MultiMap.toUnfoldable_
            <#> \(Tuple fieldKey optionKey) -> fieldKey <> "=" <> optionKey
        newOrReturningPair = if filters.newOrReturning then Just "newOrReturning=true" else Nothing
        allPairs = [pagePair, timezonePair]
            <> languagePairs <> locationPairs <> platformPairs <> fieldPairs <> Array.catMaybes
            [ ageFromPair, ageToPair, microphonePair, newOrReturningPair
            , weekdayFromPair, weekdayToPair, weekendFromPair, weekendToPair
            ]
        filterQuery = "?" <> intercalate "&" allPairs
    response <-
        Fetch.fetch_ ("/api/games/" <> handle <> "/players" <> filterQuery)
        # lmap (const Nothing)
    content <-
        case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Nothing)
        _ -> Async.left Nothing
    pure content

loadTeamProfiles :: ∀ left.
    String -> Int -> Filters -> Async left (Maybe ViewTeamProfilesByGame.OkContent)
loadTeamProfiles handle page filters = Async.unify do
    timezone <- getClientTimezone
    let pagePair = "page=" <> show page
        timezonePair = "timezone=" <> timezone
        organizationPairs = filters.organizations <#> Organization.toString <#> ("organization=" <> _)
        ageFromPair = filters.ageFrom <#> show <#> ("ageFrom=" <> _)
        ageToPair = filters.ageTo <#> show <#> ("ageTo=" <> _)
        languagePairs = filters.languages <#> ("language=" <> _)
        locationPairs = filters.locations <#> (\location -> "location=" <> location)
        microphonePair = if filters.microphone then Just "microphone=true" else Nothing
        weekdayFromPair = filters.weekdayFrom <#> ("weekdayFrom=" <> _)
        weekdayToPair = filters.weekdayTo <#> ("weekdayTo=" <> _)
        weekendFromPair = filters.weekendFrom <#> ("weekendFrom=" <> _)
        weekendToPair = filters.weekendTo <#> ("weekendTo=" <> _)
        sizePairs = filters.sizes <#> Size.toString <#> ("size=" <> _)
        platformPairs = filters.platforms <#> Platform.toString <#> ("platform=" <> _)
        fieldPairs = filters.fieldValues # MultiMap.toUnfoldable_
            <#> \(Tuple fieldKey optionKey) -> fieldKey <> "=" <> optionKey
        newOrReturningPair = if filters.newOrReturning then Just "newOrReturning=true" else Nothing
        allPairs = [pagePair, timezonePair]
            <> organizationPairs <> languagePairs <> locationPairs
            <> sizePairs <> platformPairs <> fieldPairs <> Array.catMaybes
            [ ageFromPair, ageToPair, microphonePair, newOrReturningPair
            , weekdayFromPair, weekdayToPair, weekendFromPair, weekendToPair
            ]
        filterQuery = "?" <> intercalate "&" allPairs
    response <-
        Fetch.fetch_ ("/api/games/" <> handle <> "/teams" <> filterQuery)
        # lmap (const Nothing)
    content <-
        case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Nothing)
        _ -> Async.left Nothing
    pure content

loadTab :: ∀ output left. Input -> H.HalogenM State Action ChildSlots output (Async left) Unit
loadTab { game: game @ { handle, shortTitle, fields }, tab: GameHeader.Players } = do
    { page, filters } <- H.liftEffect $ readQueryParams fields
    playerProfiles' <- H.lift $ loadPlayerProfiles handle page filters
    case playerProfiles' of
        Just playerProfiles -> do
            playerInfo <- getPlayerInfo
            H.put $ Game game playerInfo filters $ Players
                { handle: game.handle
                , profiles: playerProfiles.profiles
                , profileCount: playerProfiles.count
                , page
                , playerInfo
                }
            H.liftEffect $ setMetaTags shortTitle GameHeader.Players
        Nothing -> do
            H.put Error
            H.liftEffect $ setMetaTags handle GameHeader.Players
loadTab { game: game @ { handle, shortTitle, fields }, tab: GameHeader.Teams } = do
    { page, filters } <- H.liftEffect $ readQueryParams fields
    teamProfiles' <- H.lift $ loadTeamProfiles handle page filters
    case teamProfiles' of
        Just teamProfiles -> do
            playerInfo <- getPlayerInfo
            H.put $ Game game playerInfo filters $ Teams
                { handle: game.handle
                , profiles: teamProfiles.profiles
                , profileCount: teamProfiles.count
                , page
                , playerInfo
                }
            H.liftEffect $ setMetaTags shortTitle GameHeader.Teams
        Nothing -> do
            H.put Error
            H.liftEffect $ setMetaTags handle GameHeader.Teams

setMetaTags :: String -> GameHeader.ProfileTab -> Effect Unit
setMetaTags handleOrTitle tab =
    case tab of
    GameHeader.Players ->
        setMeta ("Players - " <> handleOrTitle <> " Team Finder | TeamTavern")
        ("Find " <> handleOrTitle <> " players looking for a team on TeamTavern, " <> indefiniteNoun handleOrTitle <> " team finding platform."
        <> " Create your own player profile and let everyone know you're looking to team up.")
    GameHeader.Teams ->
        setMeta ("Teams - " <> handleOrTitle <> " Team Finder | TeamTavern")
        ("Find " <> handleOrTitle <> " teams looking for players on TeamTavern, " <> indefiniteNoun handleOrTitle <> " team finding platform."
        <> " Create your own team profile and recruit new members for your team.")

scrollProfilesIntoView :: ∀ monad. Bind monad => MonadEffect monad => monad Unit
scrollProfilesIntoView = do
    profileCard <- Html.window >>= Window.document <#> HtmlDocument.toParentNode
        >>= ParentNode.querySelector (QuerySelector "#profiles-card")
        <#> (\element -> element >>= HtmlElement.fromElement)
        # H.liftEffect
    case profileCard of
        Just element ->
            void $ H.liftEffect $ setTimeout 0 do
                offsetTop <- round <$> HtmlElement.offsetTop element
                Html.window >>= Window.scroll 0 (offsetTop - 41 - 21)
        Nothing -> pure unit

readQueryParams
    :: ∀ fields
    .  Array { key :: String | fields }
    -> Effect { filters :: Filters, page :: Int }
readQueryParams fields = do
    searchParams <- Html.window >>= Window.location >>= Location.href
        >>= Url.url >>= Url.searchParams
    page <- Url.get "page" searchParams <#> bindFlipped Int.fromString <#> maybe 1 (max 1)
    organizations <- Url.getAll "organization" searchParams <#> Array.mapMaybe Organization.fromString
    ageFrom <- Url.get "age-from" searchParams <#> (\ageFrom -> ageFrom >>= Int.fromString)
    ageTo <- Url.get "age-to" searchParams <#> (\ageTo -> ageTo >>= Int.fromString)
    locations <- Url.getAll "location" searchParams
    languages <- Url.getAll "language" searchParams
    microphone <- Url.get "microphone" searchParams <#> isJust
    weekdayFrom <- Url.get "weekday-from" searchParams
    weekdayTo <- Url.get "weekday-to" searchParams
    weekendFrom <- Url.get "weekend-from" searchParams
    weekendTo <- Url.get "weekend-to" searchParams
    sizes <- Url.getAll "size" searchParams <#> Array.mapMaybe Size.fromString
    platforms <- Url.getAll "platform" searchParams <#> Array.mapMaybe Platform.fromString
    (fieldValues :: FieldValues) <- do
        (fieldValues :: Array { fieldKey :: String, optionKeys :: Array String}) <-
            fields # traverse \{ key } ->
                Url.getAll key searchParams <#> { fieldKey: key, optionKeys: _ }
        pure $ foldl
            (\valuesSoFar { fieldKey, optionKeys } ->
                case NonEmptyList.fromFoldable optionKeys of
                Nothing -> valuesSoFar
                Just optionKeys' -> MultiMap.insertOrReplace fieldKey optionKeys' valuesSoFar
            )
            (MultiMap.empty :: FieldValues)
            fieldValues
    newOrReturning <- Url.get "new-or-returning" searchParams <#> isJust
    pure
        { page
        , filters:
            { organizations
            , ageFrom
            , ageTo
            , locations
            , languages
            , microphone
            , weekdayFrom
            , weekdayTo
            , weekendFrom
            , weekendTo
            , sizes
            , platforms
            , fieldValues
            , newOrReturning
            }
        }

writeQueryParams :: ∀ t2. MonadEffect t2 => Filters -> t2 Unit
writeQueryParams filters = H.liftEffect do
    url <- Html.window >>= Window.location >>= Location.href >>= Url.url
    searchParams <- Url.searchParams url

    keys <- Url.keys searchParams
    foreachE keys \key -> Url.delete key searchParams

    Url.set "page" "1" searchParams
    foreachE filters.organizations \organization ->
        Url.append "organization" (Organization.toString organization) searchParams
    case filters.ageFrom of
        Nothing -> pure unit
        Just ageFrom -> Url.set "age-from" (show ageFrom) searchParams
    case filters.ageTo of
        Nothing -> pure unit
        Just ageTo -> Url.set "age-to" (show ageTo) searchParams
    foreachE filters.locations \location ->
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
    foreachE filters.sizes \size ->
        Url.append "size" (Size.toString size) searchParams
    foreachE filters.platforms \platform ->
        Url.append "platform" (Platform.toString platform) searchParams
    foreachE (MultiMap.toUnfoldable_ filters.fieldValues) \(Tuple fieldKey optionKey) ->
        Url.append fieldKey optionKey searchParams
    if filters.newOrReturning
        then Url.set "new-or-returning" "true" searchParams
        else pure unit

    href <- Url.href url
    navigate_ href

handleAction :: ∀ output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Init = do
    state <- H.get
    case state of
        Empty input -> handleAction $ Receive input
        _ -> pure unit
handleAction (Receive input) =
    loadTab input
handleAction (ApplyFilters filters) = do
    writeQueryParams filters
    scrollProfilesIntoView
    track_ "Filters apply"
handleAction ReloadPage =
    window >>= location >>= reload # H.liftEffect
handleAction (ChangePage page) = H.liftEffect do
    url <- Html.window >>= Window.location >>= Location.href >>= Url.url
    searchParams <- Url.searchParams url
    Url.set "page" (show page) searchParams
    href <- Url.href url
    navigate_ href
    scrollProfilesIntoView
    track "Profile page change" {page}
handleAction OpenPlayerPreboarding = do
    state <- H.get
    case state of
        Game game _ _ _ -> navigate (Preboarding.emptyInput (Just Boarding.Player) (Just game)) "/preboarding/start"
        _ -> pure unit
handleAction OpenTeamPreboarding = do
    state <- H.get
    case state of
        Game game _ _ _ -> navigate (Preboarding.emptyInput (Just Boarding.Team) (Just game)) "/preboarding/start"
        _ -> pure unit

component :: ∀ query output left.
    H.Component query Input output (Async left)
component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        , receive = Just <<< Receive
        }
    }

profiles :: ∀ query children left.
    Input -> HH.ComponentHTML query (profiles :: Slot___ | children) (Async left)
profiles input = HH.slot (Proxy :: _ "profiles") unit component input absurd

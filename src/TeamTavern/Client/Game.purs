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
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Halogen as H
import Halogen.HTML as HH
import Simple.JSON.Async as Json
import TeamTavern.Client.Game.GameHeader as GameHeader
import TeamTavern.Client.Game.Profiles (Input, Message(..), Slot) as Profiles
import TeamTavern.Client.Game.Profiles (playerProfiles)
import TeamTavern.Client.Profile.ProfileFilters (Filters, filterProfiles)
import TeamTavern.Client.Profile.ProfileFilters as FilterProfiles
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Server.Game.View.SendResponse as View
import TeamTavern.Server.Profile.ViewGamePlayers.SendResponse (OkContent) as Profiles

data Input = Input GameHeader.Handle GameHeader.Tab

data Action
    = Init
    | Receive Input
    | ApplyFilters Filters
    | CreateProfile
    | ChangePage Int

data State
    = Empty Input
    | Game View.OkContent GameHeader.Tab Profiles.Input
    | NotFound
    | Error

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( gameHeader :: H.Slot (Const Void) Void Unit
    , playerProfiles :: Profiles.Slot
    , filterProfiles :: FilterProfiles.Slot
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
    -> Array FilterProfiles.Field
filterableFields fields = fields # Array.mapMaybe
    case _ of
    { key, label, icon, ilk, options: Just options }
        | ilk == 2 || ilk == 3 -> Just { key, label, icon, options }
    _ -> Nothing

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Game game' tab profilesInput) = let
    gameHeader =
        HH.slot (SProxy :: SProxy "gameHeader") unit GameHeader.component
        (GameHeader.Input game'.handle game'.title tab) absurd
    in
    HH.div_
    [ gameHeader
    , filterProfiles (filterableFields game'.fields)
        (\(FilterProfiles.Apply filters) -> Just $ ApplyFilters filters)
    , playerProfiles
        profilesInput
        case _ of
        Profiles.CreateProfile -> Just CreateProfile
        Profiles.ChangePage page -> Just $ ChangePage page
    ]
render NotFound = HH.p_ [ HH.text "Game could not be found." ]
render Error = HH.p_ [ HH.text
    "There has been an error loading the game. Please try again later." ]

loadGame :: forall left. String -> Async left (Maybe View.OkContent)
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

loadProfiles :: forall left.
    String -> GameHeader.Tab -> Int -> FilterProfiles.Filters -> Async left (Maybe Profiles.OkContent)
loadProfiles handle tab page filters = Async.unify do
    timezone <- H.liftEffect getClientTimezone
    let nothingIfNull string = if String.null string then Nothing else Just string
    let tabPair =
            case tab of
            GameHeader.Players -> "ilk=1"
            GameHeader.Teams -> "ilk=2"
        pagePair = "page=" <> show page
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
        allPairs = [tabPair, pagePair, timezonePair]
            <> languagePairs <> fieldPairs <> Array.catMaybes
            [ ageFromPair, ageToPair, microphonePair
            ,  weekdayFromPair, weekdayToPair, weekendFromPair, weekendToPair
            ]
        filterQuery = "?" <> intercalate "&" allPairs
    response <-
        Fetch.fetch_ ("/api/profiles/by-handle/" <> handle <> filterQuery)
        # lmap (const Nothing)
    content <-
        case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Nothing)
        _ -> Async.left Nothing
    pure content

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Init = do
    state <- H.get
    case state of
        Empty (Input handle tab) -> do
            { gameContent, profilesContent } <-
                H.lift
                $ sequential
                $ { gameContent: _, profilesContent: _}
                <$> parallel (loadGame handle)
                <*> parallel (loadProfiles handle tab 1 FilterProfiles.emptyFilters)
            case gameContent, profilesContent of
                Just gameContent', Just profilesContent' -> do
                    H.put $ Game gameContent' tab
                        { profiles: profilesContent'.profiles
                        , profileCount: profilesContent'.count
                        , page: 1
                        , player: Nothing
                        }
                    H.liftEffect $ setMetaTags gameContent'.title tab
                _, _ -> do
                    H.put Error
                    H.liftEffect $ setMetaTags handle tab
        _ -> pure unit
handleAction (Receive (Input inputHandle inputTab)) = do
    state <- H.get
    case state of
        Game content stateTab profilesInput ->
            if content.handle == inputHandle && stateTab == inputTab
            then pure unit
            else pure unit
        _ -> pure unit
    -- case state of
    --     Game content _ _ -> do
    --         H.put $ Game content tab
    --         H.liftEffect $ setMetaTags content.title tab
    --     _ -> pure unit
    pure unit
handleAction (ApplyFilters filters) = do
    state <- H.get

    -- void $ H.query (SProxy :: SProxy "playerProfiles") unit
    --     (Profiles.Apply filters unit)
    pure unit
handleAction CreateProfile =
    pure unit
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

module TeamTavern.Client.Router (Query(..), router) where

import Prelude

import Async (Async)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Foreign (Foreign)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Content (content, singleContent, wideContent)
import TeamTavern.Client.Components.Footer (footer)
import TeamTavern.Client.Components.Footer as Footer
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Components.TopBar (topBar)
import TeamTavern.Client.Pages.About (about)
import TeamTavern.Client.Pages.DeleteAlert (deleteAlert)
import TeamTavern.Client.Pages.Game (game)
import TeamTavern.Client.Pages.GameTabs as GameTabs
import TeamTavern.Client.Pages.Games (games)
import TeamTavern.Client.Pages.Home (home)
import TeamTavern.Client.Pages.Onboarding (onboarding)
import TeamTavern.Client.Pages.Onboarding as Onboarding
import TeamTavern.Client.Pages.Player (player)
import TeamTavern.Client.Pages.PlayerProfile (playerProfile)
import TeamTavern.Client.Pages.PlayerProfile as PlayerProfile
import TeamTavern.Client.Pages.Preboarding (preboarding)
import TeamTavern.Client.Pages.Preboarding as Preboarding
import TeamTavern.Client.Pages.Privacy (privacyPolicy)
import TeamTavern.Client.Pages.Profiles.GameHeader as GameHeader
import TeamTavern.Client.Pages.Register (register)
import TeamTavern.Client.Pages.SignIn (signIn)
import TeamTavern.Client.Pages.Team (team)
import TeamTavern.Client.Pages.TeamProfile (teamProfile)
import TeamTavern.Client.Pages.TeamProfile as TeamProfile
import TeamTavern.Client.Script.Analytics (track)
import TeamTavern.Client.Script.Cookie (getPlayerNickname, hasPlayerIdCookie)
import TeamTavern.Client.Script.Navigate (navigateReplace_)
import TeamTavern.Client.Script.ReloadAds (reloadAds)
import TeamTavern.Client.Shared.Slot (SimpleSlot)
import TeamTavern.Client.Snippets.Class as HS
import Type.Proxy (Proxy(..))
import Yoga.JSON (read_)

data Query send = ChangeRoute Foreign String send

data Action = Init Foreign String

data State
    = Empty
    | Home
    | Games
    | About
    | Privacy
    | Game { handle :: String }
    | GameTabs GameTabs.Input
    | Player { nickname :: String }
    | PlayerProfile PlayerProfile.Input
    | TeamProfile TeamProfile.Input
    | Team { handle :: String }
    | Register
    | SignIn
    | Onboarding Onboarding.Input
    | Preboarding Preboarding.Input
    | NetworkN
    | NetworkN2
    | DeleteAlert
    | NotFound

type ChildSlots = Footer.ChildSlots
    ( topBar :: SimpleSlot
    , home :: SimpleSlot
    , about :: SimpleSlot
    , games :: SimpleSlot
    , game :: SimpleSlot
    , gameTabs :: SimpleSlot
    , player :: SimpleSlot
    , playerProfile :: SimpleSlot
    , team :: SimpleSlot
    , teamProfile :: SimpleSlot
    , onboarding :: SimpleSlot
    , preboarding :: SimpleSlot
    , signIn :: SimpleSlot
    , homeAnchor :: NavigationAnchor.Slot Unit
    , signInAnchor :: NavigationAnchor.Slot Unit
    , register :: SimpleSlot
    , "network-n-test" :: NavigationAnchor.Slot Unit
    , "network-n-test2" :: NavigationAnchor.Slot Unit
    , deleteAlert :: SimpleSlot
    )

topBarWithContent
    :: ∀ query children left
    .  Maybe String
    -> Array (H.ComponentHTML query (Footer.ChildSlots (topBar :: SimpleSlot | children)) (Async left))
    -> H.ComponentHTML query (Footer.ChildSlots (topBar :: SimpleSlot | children)) (Async left)
topBarWithContent handle content' = HH.div_ [ topBar handle, content content', footer ]

wideTopBarWithContent
    :: ∀ query children left
    .  Maybe String
    -> Array (H.ComponentHTML query (Footer.ChildSlots (topBar :: SimpleSlot | children)) (Async left))
    -> H.ComponentHTML query (Footer.ChildSlots (topBar :: SimpleSlot | children)) (Async left)
wideTopBarWithContent handle content' = HH.div_ [ topBar handle, wideContent content', footer ]

render :: ∀ action left. State -> H.ComponentHTML action ChildSlots (Async left)
render Empty = HH.div_ []
render Home = HH.div_ [ topBar Nothing, home, footer ]
render Games = topBarWithContent Nothing [ games ]
render About = topBarWithContent Nothing [ about ]
render Privacy = topBarWithContent Nothing [ privacyPolicy ]
render (Game input) = HH.div_ [ topBar $ Just input.handle, game input, footer ]
render (GameTabs input) = wideTopBarWithContent (Just input.handle) [ GameTabs.gameTabs input ]
render (Player input) = wideTopBarWithContent Nothing [ player input ]
render (PlayerProfile input) = topBarWithContent Nothing [ playerProfile input ]
render (TeamProfile input) = topBarWithContent Nothing [ teamProfile input ]
render (Team input) = wideTopBarWithContent Nothing [ team input ]
render Register = singleContent [ HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] [ register ] ]
render SignIn = singleContent [ HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] [ signIn ] ]
render (Onboarding input) = onboarding input
render (Preboarding input) = preboarding input
render NetworkN = HH.div_
    [ topBar Nothing
    , content
        [ HH.h3_ [ HH.text "nn_lb1" ]
        , HH.div [ HP.id "nn_lb1" ] []
        , HH.h3_ [ HH.text "nn_lb2" ]
        , HH.div [ HP.id "nn_lb2" ] []
        , HH.h3_ [ HH.text "nn_mpu1" ]
        , HH.div [ HP.id "nn_mpu1" ] []
        , HH.h3_ [ HH.text "nn_mobile_lb1_sticky" ]
        , HH.div [ HP.id "nn_mobile_lb1_sticky", HS.class_ "nn-sticky" ] []
        , HH.h3_ [ HH.text "nn_mobile_lb2" ]
        , HH.div [ HP.id "nn_mobile_lb2" ] []
        , navigationAnchor (Proxy :: _ "network-n-test2") { path: "/network-n-test2", content: HH.text "Go to test page 2" }
        ]
    , footer
    -- , HH.div [ HP.id "nn_1by1" ] []
    ]
render NetworkN2 = HH.div_
    [ topBar Nothing
    , content
        [ HH.h3_ [ HH.text "nn_lb1" ]
        , HH.div [ HP.id "nn_lb1" ] []
        , HH.h3_ [ HH.text "nn_lb2" ]
        , HH.div [ HP.id "nn_lb2" ] []
        , HH.h3_ [ HH.text "nn_mpu1" ]
        , HH.div [ HP.id "nn_mpu1" ] []
        , HH.h3_ [ HH.text "nn_mobile_lb1_sticky" ]
        , HH.div [ HP.id "nn_mobile_lb1_sticky", HS.class_ "nn-sticky" ] []
        , HH.h3_ [ HH.text "nn_mobile_lb2" ]
        , HH.div [ HP.id "nn_mobile_lb2" ] []
        , navigationAnchor (Proxy :: _ "network-n-test") { path: "/network-n-test", content: HH.text "Go to test page 1" }
        ]
    , footer
    -- , HH.div [ HP.id "nn_1by1" ] []
    ]
render DeleteAlert = singleContent [ deleteAlert ]
render NotFound = HH.p_ [ HH.text "You're fucken lost, mate." ]

just :: ∀ t5 t7. Applicative t5 => t7 -> t5 (Maybe t7)
just = pure <<< Just

nothing :: ∀ t1 t3. Applicative t1 => t1 (Maybe t3)
nothing = pure Nothing

handleAction :: ∀ action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction (Init state route) = do
    liftEffect $ track "Page view" { path: route }
    newState <- case split (Pattern "/") route of
        ["", ""] -> do
            nickname <- getPlayerNickname
            case nickname of
                Just nickname' -> (navigateReplace_ $ "/players/" <> nickname') *> nothing
                Nothing -> just Home
        ["", "about"] ->
            just About
        ["", "privacy"] ->
            just Privacy
        ["", "register"] ->
            just Register
        ["", "signin"] ->
            just SignIn
        ["", "onboarding", stepPath] ->
            let stepMaybe =
                    case stepPath of
                    "start" -> Just Onboarding.Greeting
                    "player-or-team" -> Just Onboarding.PlayerOrTeam
                    "player" -> Just Onboarding.Player
                    "team" -> Just Onboarding.Team
                    "game" -> Just Onboarding.Game
                    "player-profile" -> Just Onboarding.PlayerProfile
                    "team-profile" -> Just Onboarding.TeamProfile
                    _ -> Nothing
                (stateMaybe :: Maybe Onboarding.Input) = read_ state
            in
            case stateMaybe, stepMaybe of
                Just input, Just step -> just $ Onboarding input { step = step }
                _, _ -> navigateReplace_ "/" *> nothing
        ["", "preboarding", stepPath] ->
            let stepMaybe =
                    case stepPath of
                    "start" -> Just Preboarding.Greeting
                    "player-or-team" -> Just Preboarding.PlayerOrTeam
                    "player" -> Just Preboarding.Player
                    "team" -> Just Preboarding.Team
                    "game" -> Just Preboarding.Game
                    "player-profile" -> Just Preboarding.PlayerProfile
                    "team-profile" -> Just Preboarding.TeamProfile
                    "register" -> Just Preboarding.Register
                    _ -> Nothing
                (stateMaybe :: Maybe Preboarding.Input) = read_ state
            in do
            signedIn <- hasPlayerIdCookie
            case stateMaybe, stepMaybe of
                Just input, Just step | not signedIn -> just $ Preboarding input { step = step }
                _, _ -> navigateReplace_ "/" *> nothing
        ["", "teams", handle] ->
            just $ Team { handle }
        ["", "games"] ->
            just Games
        ["", "games", handle] -> do
            signedIn <- hasPlayerIdCookie
            if signedIn
                then (navigateReplace_ $ "/games/" <> handle <> "/players") *> nothing
                else just $ Game { handle }
        ["", "games", handle, "players" ] ->
            just $ GameTabs { handle, tab: GameHeader.Profiles GameHeader.Players }
        ["", "games", handle, "teams" ] ->
            just $ GameTabs { handle, tab: GameHeader.Profiles GameHeader.Teams }
        ["", "players", nickname] ->
            just $ Player { nickname }
        ["", "players", nickname, "profiles", handle] ->
            just $ PlayerProfile { nickname, handle }
        ["", "teams", teamHandle, "profiles", gameHandle] ->
            just $ TeamProfile { teamHandle, gameHandle }
        ["", "network-n-test"] ->
            just $ NetworkN
        ["", "network-n-test2"] ->
            just $ NetworkN2
        ["", "remove-alert" ] ->
            just $ DeleteAlert
        _ ->
            navigateReplace_ "/" *> nothing
    case newState of
        Just newState' -> do
            H.put newState'
            case newState' of
                NetworkN -> reloadAds
                NetworkN2 -> reloadAds
                _ -> pure unit
        Nothing -> pure unit

handleQuery
    :: ∀ send action output wut left
    .  Query send
    -> H.HalogenM State action wut output (Async left) (Maybe send)
handleQuery (ChangeRoute state route send) = do
    handleAction (Init state route)
    pure $ Just send

router :: ∀ input output left.
    Foreign -> String -> H.Component Query input output (Async left)
router state route = H.mkComponent
    { initialState: const Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleQuery = handleQuery
        , handleAction = handleAction
        , initialize = Just $ Init state route
        }
    }

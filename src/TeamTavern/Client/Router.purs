module TeamTavern.Client.Router (Query(..), router) where

import Prelude

import Async (Async)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Foreign (Foreign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Simple.JSON (read_)
import TeamTavern.Client.Components.Content (content, singleContent, wideContent)
import TeamTavern.Client.Components.Footer (footer)
import TeamTavern.Client.Components.Footer as Footer
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Components.TopBar (topBar)
import TeamTavern.Client.Components.TopBar as TopBar
import TeamTavern.Client.Pages.About (about)
import TeamTavern.Client.Pages.About as About
import TeamTavern.Client.Pages.Game (game)
import TeamTavern.Client.Pages.Game as Game
import TeamTavern.Client.Pages.Games (games)
import TeamTavern.Client.Pages.Games as Games
import TeamTavern.Client.Pages.Home (home)
import TeamTavern.Client.Pages.Home as Home
import TeamTavern.Client.Pages.Onboarding (onboarding)
import TeamTavern.Client.Pages.Onboarding as Onboarding
import TeamTavern.Client.Pages.Player (player)
import TeamTavern.Client.Pages.Player as Player
import TeamTavern.Client.Pages.Preboarding (preboarding)
import TeamTavern.Client.Pages.Preboarding as Preboarding
import TeamTavern.Client.Pages.Profiles as Profiles
import TeamTavern.Client.Pages.Profiles.GameHeader as GameHeader
import TeamTavern.Client.Pages.Register (register)
import TeamTavern.Client.Pages.Register as Register
import TeamTavern.Client.Pages.SignIn (signIn)
import TeamTavern.Client.Pages.SignIn as SignIn
import TeamTavern.Client.Pages.Team (team)
import TeamTavern.Client.Pages.Team as Team
import TeamTavern.Client.Script.Cookie (getPlayerNickname, hasPlayerIdCookie)
import TeamTavern.Client.Script.Navigate (navigateReplace_)

data Query send = ChangeRoute Foreign String send

data Action = Init Foreign String

data State
    = Empty
    | Home
    | Games
    | About
    | Game { handle :: String }
    | Profiles GameHeader.Handle GameHeader.Tab
    | Player { nickname :: String }
    | Team { handle :: String }
    | Register
    | SignIn
    | Onboarding Onboarding.Input
    | Preboarding Preboarding.Input
    | NotFound

type ChildSlots = Footer.ChildSlots
    ( topBar :: TopBar.Slot Unit
    , home :: Home.Slot Unit
    , about :: About.Slot
    , games :: Games.Slot Unit
    , game :: Game.Slot
    , profiles :: Profiles.Slot Unit
    , player :: Player.Slot
    , team :: Team.Slot
    , onboarding :: Onboarding.Slot
    , preboarding :: Preboarding.Slot
    , signIn :: SignIn.Slot Unit
    , homeAnchor :: NavigationAnchor.Slot Unit
    , signInAnchor :: NavigationAnchor.Slot Unit
    , register :: Register.Slot Unit
    )

topBarWithContent
    :: forall query children left
    .  Array (H.ComponentHTML query (Footer.ChildSlots (topBar :: TopBar.Slot Unit | children)) (Async left))
    -> H.ComponentHTML query (Footer.ChildSlots (topBar :: TopBar.Slot Unit | children)) (Async left)
topBarWithContent content' = HH.div_ [ topBar, content content', footer ]

wideTopBarWithContent
    :: forall query children left
    .  Array (H.ComponentHTML query (Footer.ChildSlots (topBar :: TopBar.Slot Unit | children)) (Async left))
    -> H.ComponentHTML query (Footer.ChildSlots (topBar :: TopBar.Slot Unit | children)) (Async left)
wideTopBarWithContent content' = HH.div_ [ topBar, wideContent content', footer ]

render :: forall action left. State -> H.ComponentHTML action ChildSlots (Async left)
render Empty = HH.div_ []
render Home = HH.div_ [ topBar, home, footer ]
render Games = topBarWithContent [ games ]
render About = topBarWithContent [ about ]
render (Game input) = HH.div_ [ topBar, game input, footer ]
render (Profiles handle tab) = wideTopBarWithContent [ Profiles.profiles handle tab ]
render (Player input) = topBarWithContent [ player input ]
render (Team input) = topBarWithContent [ team input ]
render Register = singleContent [ HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] [ register ] ]
render SignIn = singleContent [ HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] [ signIn ] ]
render (Onboarding input) = onboarding input
render (Preboarding input) = preboarding input
render NotFound = HH.p_ [ HH.text "You're fucken lost, mate." ]

just :: forall t5 t7. Applicative t5 => t7 -> t5 (Maybe t7)
just = pure <<< Just

nothing :: forall t1 t3. Applicative t1 => t1 (Maybe t3)
nothing = pure Nothing

handleAction :: forall action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction (Init state route) = do
    newState <- case split (Pattern "/") route of
        ["", ""] -> do
            nickname <- getPlayerNickname
            case nickname of
                Just nickname' -> (navigateReplace_ $ "/players/" <> nickname') *> nothing
                Nothing -> just Home
        ["", "about"] ->
            just About
        ["", "register"] ->
            just Register
        ["", "signin"] ->
            just SignIn
        ["", "onboarding", step] ->
            let step' =
                    case step of
                    "start" -> Just Onboarding.Greeting
                    "player-or-team" -> Just Onboarding.PlayerOrTeam
                    "player" -> Just Onboarding.Player
                    "team" -> Just Onboarding.Team
                    "game" -> Just Onboarding.Game
                    "player-profile" -> Just Onboarding.PlayerProfile
                    "team-profile" -> Just Onboarding.TeamProfile
                    _ -> Nothing
            in
            case (read_ state :: Maybe Onboarding.Input), step' of
            Just input, Just step'' -> just $ Onboarding input { step = step'' }
            _, _ -> navigateReplace_ "/" *> nothing
        ["", "preboarding", step] ->
            let step' =
                    case step of
                    "start" -> Just Preboarding.Greeting
                    "player-or-team" -> Just Preboarding.PlayerOrTeam
                    "player" -> Just Preboarding.Player
                    "team" -> Just Preboarding.Team
                    "game" -> Just Preboarding.Game
                    "player-profile" -> Just Preboarding.PlayerProfile
                    "team-profile" -> Just Preboarding.TeamProfile
                    "register" -> Just Preboarding.Register
                    _ -> Nothing
            in
            case (read_ state :: Maybe Preboarding.Input), step' of
            Just input, Just step'' -> just $ Preboarding input { step = step'' }
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
            just $ Profiles handle GameHeader.Players
        ["", "games", handle, "teams" ] ->
            just $ Profiles handle GameHeader.Teams
        ["", "players", nickname] ->
            just $ Player { nickname }
        _ ->
            navigateReplace_ "/" *> nothing
    case newState of
        Just newState' -> H.put newState'
        Nothing -> pure unit

handleQuery
    :: forall send action output wut left
    .  Query send
    -> H.HalogenM State action wut output (Async left) (Maybe send)
handleQuery (ChangeRoute state route send) = do
    handleAction (Init state route)
    pure $ Just send

router :: forall input output left.
    Foreign -> String -> H.Component HH.HTML Query input output (Async left)
router state route = H.mkComponent
    { initialState: const Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleQuery = handleQuery
        , handleAction = handleAction
        , initialize = Just $ Init state route
        }
    }

module TeamTavern.Client.Router (Query(..), router) where

import Prelude

import Async (Async)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Foreign (Foreign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Simple.JSON (read)
import TeamTavern.Client.Components.Content (content, singleContent, wideContent)
import TeamTavern.Client.Components.Footer (footer)
import TeamTavern.Client.Components.Footer as Footer
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Components.Password.ForgotPassword (forgotPassword)
import TeamTavern.Client.Components.Password.ForgotPassword as ForgotPassword
import TeamTavern.Client.Components.Password.ResetPassword (resetPassword)
import TeamTavern.Client.Components.Password.ResetPassword as ResetPassword
import TeamTavern.Client.Components.Password.ResetPasswordSent (resetPasswordSent)
import TeamTavern.Client.Components.Password.ResetPasswordSuccess (resetPasswordSuccess)
import TeamTavern.Client.Components.TopBar (topBar)
import TeamTavern.Client.Components.TopBar as TopBar
import TeamTavern.Client.Components.Welcome (welcome)
import TeamTavern.Client.Components.Welcome as Welcome
import TeamTavern.Client.Pages.About (about)
import TeamTavern.Client.Pages.About as About
import TeamTavern.Client.Pages.Conversation (conversation)
import TeamTavern.Client.Pages.Conversation as Conversation
import TeamTavern.Client.Pages.Conversations (conversations)
import TeamTavern.Client.Pages.Conversations as Conversations
import TeamTavern.Client.Pages.Game (game)
import TeamTavern.Client.Pages.Game as Game
import TeamTavern.Client.Pages.Games (games)
import TeamTavern.Client.Pages.Games as Games
import TeamTavern.Client.Pages.Home (home)
import TeamTavern.Client.Pages.Home as Home
import TeamTavern.Client.Pages.Player (player)
import TeamTavern.Client.Pages.Player as Player
import TeamTavern.Client.Pages.Profiles as Profiles
import TeamTavern.Client.Pages.Profiles.GameHeader as GameHeader
import TeamTavern.Client.Pages.Register (register)
import TeamTavern.Client.Pages.Register as Register
import TeamTavern.Client.Pages.SignIn (signIn)
import TeamTavern.Client.Pages.SignIn as SignIn
import TeamTavern.Client.Pages.Team (team)
import TeamTavern.Client.Pages.Team as Team
import TeamTavern.Client.Pages.Onboarding (onboarding)
import TeamTavern.Client.Pages.Onboarding as Onboarding
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
    | Conversations
    | Conversation String
    | Team { handle :: String }
    | Register
    | SignIn
    | Onboarding Onboarding.Input
    | ForgotPassword
    | ResetPasswordSent { email :: String }
    | ResetPassword
    | ResetPasswordSuccess
    | Welcome Welcome.Input
    | NotFound

type ChildSlots = Footer.ChildSlots
    ( topBar :: TopBar.Slot Unit
    , home :: Home.Slot Unit
    , about :: About.Slot
    , games :: Games.Slot Unit
    , game :: Game.Slot
    , profiles :: Profiles.Slot Unit
    , player :: Player.Slot
    , conversations :: Conversations.Slot
    , conversation :: Conversation.Slot
    , team :: Team.Slot
    , onboarding :: Onboarding.Slot
    , signIn :: SignIn.Slot Unit
    , homeAnchor :: NavigationAnchor.Slot Unit
    , signInAnchor :: NavigationAnchor.Slot Unit
    , register :: Register.Slot Unit
    , forgotPassword :: ForgotPassword.Slot
    , resetPassword :: ResetPassword.Slot
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

render :: forall action left.
    State -> H.ComponentHTML action ChildSlots (Async left)
render Empty = HH.div_ []
render Home = HH.div_ [ topBar, home, footer ]
render Games = topBarWithContent [ games ]
render About = topBarWithContent [ about ]
render (Game input) = HH.div_ [ topBar, game input, footer ]
render (Profiles handle tab) = wideTopBarWithContent [ Profiles.profiles handle tab ]
render (Player input) = topBarWithContent [ player input ]
render Conversations = topBarWithContent [ conversations ]
render (Conversation nickname) = topBarWithContent [ conversation nickname]
render (Team input) = topBarWithContent [ team input ]
render Register = singleContent [ HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] [ register ] ]
render SignIn = singleContent [ HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] [ signIn ] ]
render (Onboarding input) = onboarding input
render ForgotPassword = singleContent [ HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] [ forgotPassword ] ]
render (ResetPasswordSent resetPasswordData) = singleContent [ resetPasswordSent resetPasswordData ]
render ResetPassword = singleContent [ HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] [ resetPassword ] ]
render ResetPasswordSuccess = singleContent [ resetPasswordSuccess ]
render (Welcome welcomeData) = singleContent [ welcome welcomeData ]
render NotFound = HH.p_ [ HH.text "You're fucken lost, mate." ]

just :: forall t5 t7. Applicative t5 => t7 -> t5 (Maybe t7)
just = pure <<< Just

nothing :: forall t1 t3. Applicative t1 => t1 (Maybe t3)
nothing = pure Nothing

handleAction :: forall action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction (Init state route) = do
    newState <- H.liftEffect $ case split (Pattern "/") route of
        ["", ""] ->
            just Home
        ["", "about"] ->
            just About
        ["", "register"] ->
            just Register
        ["", "signin"] ->
            just SignIn
        ["", "onboarding", "start"] ->
            -- case read state of
            -- Right ({ firstSignIn: true } :: { firstSignIn :: Boolean}) ->
                just $ Onboarding { step: Onboarding.Greeting }
            -- _ -> navigate_ "/" *> nothing
        ["", "onboarding", "player-or-team"] ->
            just $ Onboarding { step: Onboarding.PlayerOrTeam }
        ["", "onboarding", "player"] ->
            just $ Onboarding { step: Onboarding.PlayerDetails }
        ["", "onboarding", "team"] ->
            just $ Onboarding { step: Onboarding.TeamDetails }
        ["", "onboarding", "game"] ->
            just $ Onboarding { step: Onboarding.Game }
        ["", "onboarding", "player-profile"] ->
            just $ Onboarding { step: Onboarding.PlayerProfileDetails }
        ["", "onboarding", "team-profile"] ->
            just $ Onboarding { step: Onboarding.TeamProfileDetails }
        ["", "forgot-password"] ->
            just ForgotPassword
        ["", "reset-password-sent"] ->
            case read state of
            Right email -> just $ ResetPasswordSent email
            Left _ -> navigateReplace_ "/" *> nothing
        ["", "reset-password"] ->
            just ResetPassword
        ["", "reset-password-success"] ->
            just ResetPasswordSuccess
        ["", "welcome"] ->
            case read state of
            Right identifiers -> just $ Welcome identifiers
            Left _ -> navigateReplace_ "/" *> nothing
        ["", "conversations"] ->
            just $ Conversations
        ["", "conversations", nickname] ->
            just $ Conversation nickname
        ["", "teams", handle] ->
            just $ Team { handle }
        ["", "games"] ->
            just Games
        ["", "games", handle] ->
            just $ Game { handle }
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

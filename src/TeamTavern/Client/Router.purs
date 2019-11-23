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
import TeamTavern.Client.Components.Account.AccountHeader as AccountHeader
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Components.RegisterForm (registerForm)
import TeamTavern.Client.Components.RegisterForm as RegisterForm
import TeamTavern.Client.Components.TopBar (topBar)
import TeamTavern.Client.Components.TopBar as TopBar
import TeamTavern.Client.Components.Welcome (welcome)
import TeamTavern.Client.Components.WelcomeBanner as WelcomeBanner
import TeamTavern.Client.Game (game)
import TeamTavern.Client.Game as Game
import TeamTavern.Client.Game.GameHeader as GameHeader
import TeamTavern.Client.Home (home)
import TeamTavern.Client.Home as Home
import TeamTavern.Client.Home.Games as Games
import TeamTavern.Client.Pages.Account (account)
import TeamTavern.Client.Pages.Account as Account
import TeamTavern.Client.Player (player)
import TeamTavern.Client.Player as Player
import TeamTavern.Client.Script.Navigate (navigateReplace_, navigate_)
import TeamTavern.Client.SignIn (signIn)
import TeamTavern.Client.SignIn as SignIn

data Query send = ChangeRoute Foreign String send

data Action = Init Foreign String

data State
    = Empty
    | Home
    | Account AccountHeader.Tab
    | Game GameHeader.Handle GameHeader.Tab
    | Player String
    | Register
    | SignIn
    | Welcome { email :: String, nickname :: String, emailSent :: Boolean }
    | NotFound

type ChildSlots =
    ( topBar :: TopBar.Slot Unit
    , home :: Home.Slot Unit
    , welcomeBanner :: WelcomeBanner.Slot Unit
    , account :: Account.Slot
    , games :: Games.Slot Unit
    , game :: Game.Slot Unit
    , player :: Player.Slot Unit
    , signIn :: SignIn.Slot Unit
    , homeAnchor :: NavigationAnchor.Slot Unit
    , signInAnchor :: NavigationAnchor.Slot Unit
    , registerForm :: RegisterForm.Slot Unit
    )

topBarWithContent
    :: forall query children left
    .  Array (H.ComponentHTML query (topBar :: TopBar.Slot Unit | children) (Async left))
    -> H.ComponentHTML query (topBar :: TopBar.Slot Unit | children ) (Async left)
topBarWithContent content =
    HH.div_ [ topBar, HH.div [ HP.class_ $ HH.ClassName "content" ] content ]

singleContent :: forall slots query.
    Array (HH.HTML slots query) -> HH.HTML slots query
singleContent = HH.div [ HP.class_ $ HH.ClassName "single-content" ]

render :: forall action left.
    State -> H.ComponentHTML action ChildSlots (Async left)
render Empty = HH.div_ []
render Home = HH.div_ [ topBar, home ]
render (Game handle tab) = topBarWithContent [ game handle tab ]
render (Account tab) = topBarWithContent [ account tab ]
render (Player nickname) = topBarWithContent [ player nickname ]
render Register = singleContent [ HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] [ registerForm ] ]
render SignIn = singleContent [ HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] [ signIn ] ]
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
        ["", "register"] ->
            just Register
        ["", "signin"] ->
            just SignIn
        ["", "welcome"] ->
            case read state of
            Right identifiers -> just $ Welcome identifiers
            Left _ -> navigate_ "/" *> nothing
        ["", "account"] ->
            (navigateReplace_ $ "/account/profiles") *> nothing
        ["", "account", "profiles"] ->
            just $ Account AccountHeader.Profiles
        ["", "account", "conversations"] ->
            just $ Account AccountHeader.Conversations
        ["", "games", handle] ->
            (navigateReplace_ $ "/games/" <> handle <> "/players") *> nothing
        ["", "games", handle, "players" ] ->
            just $ Game handle GameHeader.Players
        ["", "games", handle, "teams" ] ->
            just $ Game handle GameHeader.Teams
        ["", "players", nickname] ->
            just $ Player nickname
        _ ->
            navigate_ "/" *> nothing
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

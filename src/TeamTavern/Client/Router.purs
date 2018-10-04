module TeamTavern.Client.Router where

import Prelude

import Async (Async)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Symbol (SProxy(..))
import Foreign (Foreign)
import Halogen as H
import Halogen.HTML as HH
import Simple.JSON (read)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Components.ProfilesByGame (profilesByGame)
import TeamTavern.Client.Components.ProfilesByGame as ProfilesByGame
import TeamTavern.Client.Components.TopBar (topBar)
import TeamTavern.Client.Components.TopBar as TopBar
import TeamTavern.Client.CreateGame (createGame)
import TeamTavern.Client.CreateGame as CreateGame
import TeamTavern.Client.EditGame (editGame)
import TeamTavern.Client.EditGame as EditGame
import TeamTavern.Client.EditPlayer (editPlayer)
import TeamTavern.Client.EditPlayer as EditPlayer
import TeamTavern.Client.Game (game)
import TeamTavern.Client.Game as Game
import TeamTavern.Client.Home (home)
import TeamTavern.Client.Home as Home
import TeamTavern.Client.Player (player)
import TeamTavern.Client.Player as Player
import TeamTavern.Client.Register (register)
import TeamTavern.Client.Register as Register
import TeamTavern.Client.SignIn (signIn)
import TeamTavern.Client.SignIn as SignIn
import TeamTavern.Client.SignInCode (signInCode)
import TeamTavern.Client.SignInCode as SignInCode

data Query send = ChangeRoute Foreign String send

data State
    = Empty
    | Home
    | Game String
    | CreateGame
    | EditGame String
    | Player String
    | EditPlayer String
    | Register
    | SignIn
    | Code
    | Welcome { email :: String, nickname :: String, emailSent :: Boolean }
    | CodeSent { email :: String, nickname :: String }
    | NotFound

type ChildSlots =
    ( topBar :: TopBar.Slot Unit
    , home :: Home.Slot Unit
    , game :: Game.Slot Unit
    , createGame :: CreateGame.Slot Unit
    , editGame :: EditGame.Slot Unit
    , profiles :: ProfilesByGame.Slot Unit
    , player :: Player.Slot Unit
    , editPlayer :: EditPlayer.Slot Unit
    , register :: Register.Slot Unit
    , signIn :: SignIn.Slot Unit
    , signInCode :: SignInCode.Slot Unit
    , signInAnchor :: NavigationAnchor.Slot Unit
    )

render :: forall void. State -> H.ComponentHTML Query ChildSlots (Async void)
render Empty = HH.div_ []
render Home = HH.div_ [ topBar, home ]
render (Game handle) = HH.div_ [ topBar, game handle, profilesByGame handle ]
render CreateGame = HH.div_ [ topBar, createGame ]
render (EditGame handle) = HH.div_ [ topBar, editGame handle ]
render (Player nickname) = HH.div_ [ topBar, player nickname ]
render (EditPlayer nickname) = HH.div_ [ topBar, editPlayer nickname ]
render Register = register
render SignIn = signIn
render Code = signInCode
render (Welcome { email, nickname, emailSent }) = HH.div_
    [ HH.h3_ [ HH.text $ "Welcome to TeamTavern, " <> nickname <> "!" ]
    , HH.p_ [ if emailSent
        then HH.text $ "Registration email with your sign in code has been sent to " <> email
        else HH.text $ "Registration email has NOT been sent to " <> email ]
    , HH.p_ [ navigationAnchor (SProxy :: SProxy "signInAnchor")
        { path: "/signin", text: "Sign in" } ]
    ]
render (CodeSent { email, nickname }) = HH.div_
    [ HH.h3_ [ HH.text $ "Hello, " <> nickname <> "!" ]
    , HH.p_ [ HH.text $ "An email with your sign in code has been sent to " <> email ]
    , HH.p_ [ navigationAnchor (SProxy :: SProxy "signInAnchor")
        { path: "/signin", text: "Sign in" } ]
    ]
render NotFound = HH.p_ [ HH.text "You're fucken lost, mate." ]

eval :: forall void.
    Query ~> H.HalogenM State Query ChildSlots Void (Async void)
eval (ChangeRoute state route send) = do
    H.put case route of
        "/" -> Home
        "/register" -> Register
        "/signin" -> SignIn
        "/code" -> Code
        "/welcome" ->
            case read state of
            Right identifiers -> Welcome identifiers
            Left _ -> Home
        "/codesent" ->
            case read state of
            Right identifiers -> CodeSent identifiers
            Left _ -> Home
        other -> let
            parts = split (Pattern "/") other
            in
            case parts of
            ["", "games", "create"] -> CreateGame
            ["", "games", handle, "edit"] -> EditGame handle
            ["", "games", handle] -> Game handle
            ["", "players", nickname, "edit"] -> EditPlayer nickname
            ["", "players", nickname] -> Player nickname
            _ -> NotFound
    pure send

router :: forall input void.
    Foreign -> String -> H.Component HH.HTML Query input Void (Async void)
router state route = H.component
    { initialState: const Empty
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ ChangeRoute state route unit
    , finalizer: Nothing
    }

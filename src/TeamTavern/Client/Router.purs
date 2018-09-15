module TeamTavern.Client.Router where

import Prelude

import Async (Async)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Symbol (SProxy(..))
import Foreign (Foreign)
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.ProfilesByGame (profilesByGame)
import TeamTavern.Client.Components.ProfilesByGame as ProfilesByGame
import TeamTavern.Client.Components.TopBar (topBar)
import TeamTavern.Client.Components.TopBar as TopBar
import TeamTavern.Client.Game (game)
import TeamTavern.Client.Game as Game
import TeamTavern.Client.Home (home)
import TeamTavern.Client.Home as Home
import TeamTavern.Client.Player (player)
import TeamTavern.Client.Player as Player
import TeamTavern.Client.Register (register)
import TeamTavern.Client.Register as Register

data Query send = ChangeRoute Foreign String send

data State
    = Empty
    | Home
    | Game String
    | Player String
    | Register
    -- | Welcome { email :: String, nickname :: String, emailSent :: Boolean }
    -- | Code
    -- | CodeSent { email :: String, nickname :: String }
    -- | SignIn
    | NotFound

type ChildSlots =
  ( topBar :: TopBar.Slot Unit
  , home :: Home.Slot Unit
  , game :: Game.Slot Unit
  , profiles :: ProfilesByGame.Slot Unit
  , player :: Player.Slot Unit
  , register :: Register.Slot Unit
--   , signIn :: SignIn.Slot Unit
--   , code :: Code.Slot Unit
--   , signInAnchor :: NavigationAnchor.Slot Unit
  )

_signIn = SProxy :: SProxy "signIn"

_register = SProxy :: SProxy "register"

_code = SProxy :: SProxy "code"

_player = SProxy :: SProxy "player"

_signInAnchor = SProxy :: SProxy "signInAnchor"

render :: forall void. State -> H.ComponentHTML Query ChildSlots (Async void)
render Empty = HH.div_ []
render Home = HH.div_ [ topBar, home ]
render (Game handle) = HH.div_ [ topBar, game handle, profilesByGame handle ]
render (Player nickname) = HH.div_ [ topBar, player nickname ]
render Register = register
-- render SignIn = HH.slot _signIn unit SignIn.signIn unit absurd
-- render (Welcome { email, nickname, emailSent }) = HH.div_
--     [ HH.p_ [ HH.text $ "Welcome to TeamTavern, " <> nickname <> "!" ]
--     , HH.p_ [ if emailSent
--         then HH.text $ "Registration email with your sign in code has been sent to " <> email
--         else HH.text $ "Registration email has NOT been sent to " <> email ]
--     , HH.p_ [ HH.slot _signInAnchor unit navigationAnchor { path: "/signin", text: "Sign in" } absurd ]
--     ]
-- render Code = HH.slot _code unit Code.code unit absurd
-- render (CodeSent { email, nickname }) = HH.div_
--     [ HH.p_ [ HH.text $ "Hello, " <> nickname <> "!" ]
--     , HH.p_ [ HH.text $ "An email with your sign in code has been sent to " <> email ]
--     , HH.p_ [ HH.slot _signInAnchor unit navigationAnchor { path: "/signin", text: "Sign in" } absurd ]
--     ]
-- render (Player playerInfo nickname) = HH.div_
--     [ HH.slot _topBar unit topBar playerInfo absurd
--     , HH.slot _player unit Player.player nickname absurd
--     ]
render NotFound = HH.p_ [ HH.text "You're fucken lost, mate." ]

eval :: forall void.
    Query ~> H.HalogenM State Query ChildSlots Void (Async void)
eval (ChangeRoute state route send) = do
    H.put case route of
        "/" -> Home
        "/register" -> Register
        -- "/signin" -> SignIn
        -- "/welcome" ->
        --     case read state of
        --     Left _ -> Home playerInfo
        --     Right identifiers -> Welcome identifiers
        -- "/code" -> Code
        -- "/codesent" ->
        --     case read state of
        --     Left _ -> Home playerInfo
        --     Right identifiers -> CodeSent identifiers
        -- "/players/bklaric" -> Player playerInfo "bklaric"
        other -> let
            parts = split (Pattern "/") other
            in
            case parts of
            ["", "games", handle] -> Game handle
            ["", "players", nickname] -> Player nickname
            _ -> NotFound
    pure send

router :: forall input void.
    Foreign -> String -> H.Component HH.HTML Query input Void (Async void)
router state route = H.component
    { initialState: const Home
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ ChangeRoute state route unit
    , finalizer: Nothing
    }

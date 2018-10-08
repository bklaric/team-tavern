module TeamTavern.Client.Router where

import Prelude

import Async (Async)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Foreign (Foreign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Simple.JSON (read)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Components.ProfilesByGame (profilesByGame)
import TeamTavern.Client.Components.ProfilesByGame as ProfilesByGame
import TeamTavern.Client.Components.TopBar (topBar)
import TeamTavern.Client.Components.TopBar as TopBar
import TeamTavern.Client.CreateGame (createGame)
import TeamTavern.Client.CreateGame as CreateGame
import TeamTavern.Client.CreateProfile (createProfile)
import TeamTavern.Client.CreateProfile as CreateProfile
import TeamTavern.Client.EditGame (editGame)
import TeamTavern.Client.EditGame as EditGame
import TeamTavern.Client.EditPlayer (editPlayer)
import TeamTavern.Client.EditPlayer as EditPlayer
import TeamTavern.Client.EditProfile (editProfile)
import TeamTavern.Client.EditProfile as EditProfile
import TeamTavern.Client.Game (game)
import TeamTavern.Client.Game as Game
import TeamTavern.Client.Home (home)
import TeamTavern.Client.Home as Home
import TeamTavern.Client.Player (player)
import TeamTavern.Client.Player as Player
import TeamTavern.Client.Register (register)
import TeamTavern.Client.Register as Register
import TeamTavern.Client.Script.Navigate (navigate_)
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
    | CreateProfile String
    | EditProfile String String
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
    , createProfile :: CreateProfile.Slot Unit
    , editProfile :: EditProfile.Slot Unit
    , player :: Player.Slot Unit
    , editPlayer :: EditPlayer.Slot Unit
    , register :: Register.Slot Unit
    , signIn :: SignIn.Slot Unit
    , signInCode :: SignInCode.Slot Unit
    , homeAnchor :: NavigationAnchor.Slot Unit
    , signInAnchor :: NavigationAnchor.Slot Unit
    , signInCodeAnchor :: NavigationAnchor.Slot Unit
    )

topBarWithContent
    :: forall query children monad
    .  MonadEffect monad
    => Array
        (H.ComponentHTML query ( topBar :: TopBar.Slot Unit | children) monad)
    -> H.ComponentHTML query ( topBar :: TopBar.Slot Unit | children ) monad
topBarWithContent content =
    HH.div_ [ topBar, HH.div [ HP.id_ "content" ] content ]

singleContent :: forall slots query.
    Array (HH.HTML slots query) -> HH.HTML slots query
singleContent = HH.div [ HP.id_ "single-content" ]

render :: forall void. State -> H.ComponentHTML Query ChildSlots (Async void)
render Empty = HH.div_ []
render Home = topBarWithContent [ home ]
render (Game handle) = topBarWithContent [ game handle, profilesByGame handle ]
render CreateGame = topBarWithContent [ createGame ]
render (EditGame handle) = topBarWithContent [ editGame handle ]
render (Player nickname) = topBarWithContent [ player nickname ]
render (EditPlayer nickname) = topBarWithContent [ editPlayer nickname ]
render (CreateProfile handle) = topBarWithContent [ createProfile handle ]
render (EditProfile nickname handle) = topBarWithContent [ editProfile nickname handle]
render Register = singleContent [ register ]
render SignIn = singleContent [ signIn ]
render Code = singleContent [ signInCode ]
render (Welcome { email, nickname, emailSent }) = singleContent
    [ HH.div_ $ join
        [ pure $ HH.h3_ [ HH.text $ "Welcome to TeamTavern, " <> nickname <> "!" ]
        , if emailSent
            then
                [ HH.p_
                    [ HH.text $ "A registration email "
                        <> "with your sign in code has been sent to "
                    , HH.strong_ [ HH.text email ]
                    , HH.text "."
                    ]
                , navigationAnchor (SProxy :: SProxy "signInAnchor")
                    { path: "/signin", text: "Sign in" }
                , navigationAnchor (SProxy :: SProxy "homeAnchor")
                    { path: "/", text: "Home" }
                ]
            else
                [ HH.p_
                    [ HH.text $ "Unfortunately, we're having some issues sending "
                        <> "a registration email with your sign in code to "
                    , HH.strong_ [ HH.text email ]
                    , HH.text ". Please try requesting a sign in code again later."
                    ]
                , navigationAnchor (SProxy :: SProxy "signInAnchor")
                    { path: "/code", text: "Get a sign in code" }
                , navigationAnchor (SProxy :: SProxy "homeAnchor")
                    { path: "/", text: "Home" }
                ]
        ]
    ]
render (CodeSent { email, nickname }) = singleContent
    [ HH.div_
        [ HH.h3_ [ HH.text $ "Hello, " <> nickname <> "!" ]
        , HH.p_
            [ HH.text $ "An email with your sign in code has been sent to "
            , HH.strong_ [ HH.text email ]
            , HH.text "."
            ]
        , navigationAnchor (SProxy :: SProxy "signInAnchor")
            { path: "/signin", text: "Sign in" }
        ]
    ]
render NotFound = HH.p_ [ HH.text "You're fucken lost, mate." ]

just :: forall t5 t7. Applicative t5 => t7 -> t5 (Maybe t7)
just = pure <<< Just

nothing :: forall t1 t3. Applicative t1 => t1 (Maybe t3)
nothing = pure Nothing

eval :: forall void.
    Query ~> H.HalogenM State Query ChildSlots Void (Async void)
eval (ChangeRoute state route send) = do
    newState <- H.liftEffect $ case split (Pattern "/") route of
        ["", ""] ->
            just Home
        ["", "register"] ->
            just Register
        ["", "signin"] ->
            just SignIn
        ["", "code"] ->
            just Code
        ["", "welcome"] ->
            case read state of
            Right identifiers -> just $ Welcome identifiers
            Left _ -> navigate_ "/" *> nothing
        ["", "codesent"] ->
            case read state of
            Right identifiers -> just $ CodeSent identifiers
            Left _ -> navigate_ "/" *> nothing
        ["", "games", "create"] ->
            just $ CreateGame
        ["", "games", handle, "edit"] ->
            just $ EditGame handle
        ["", "games", handle] ->
            just $ Game handle
        ["", "games", handle, "profiles", "create"] ->
            just $ CreateProfile handle
        ["", "games", handle, "profiles", nickname, "edit"] ->
            just $ EditProfile nickname handle
        ["", "players", nickname, "edit"] ->
            just $ EditPlayer nickname
        ["", "players", nickname] ->
            just $ Player nickname
        _ ->
            just NotFound
    case newState of
        Just newState' -> H.put newState'
        Nothing -> pure unit
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

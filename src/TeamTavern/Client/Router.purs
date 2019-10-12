module TeamTavern.Client.Router where

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
import TeamTavern.Client.Components.Games (games)
import TeamTavern.Client.Components.Games as Games
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Components.TopBar (topBar)
import TeamTavern.Client.Components.TopBar as TopBar
import TeamTavern.Client.Components.Welcome (welcome)
import TeamTavern.Client.Components.WelcomeBanner (welcomeBanner)
import TeamTavern.Client.Components.WelcomeBanner as WelcomeBanner
import TeamTavern.Client.Game (game)
import TeamTavern.Client.Game as Game
import TeamTavern.Client.Player (player)
import TeamTavern.Client.Player as Player
import TeamTavern.Client.Register (register)
import TeamTavern.Client.Register as Register
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.SignIn (signIn)
import TeamTavern.Client.SignIn as SignIn

data Query send = ChangeRoute Foreign String send

data Action = Init Foreign String

data State
    = Empty
    | Home
    | Game String
    | Player String
    | Register
    | SignIn
    | Welcome { email :: String, nickname :: String, emailSent :: Boolean }
    | NotFound

type ChildSlots =
    ( topBar :: TopBar.Slot Unit
    , welcomeBanner :: WelcomeBanner.Slot Unit
    , games :: Games.Slot Unit
    , game :: Game.Slot Unit
    , player :: Player.Slot Unit
    , register :: Register.Slot Unit
    , signIn :: SignIn.Slot Unit
    , homeAnchor :: NavigationAnchor.Slot Unit
    , signInAnchor :: NavigationAnchor.Slot Unit
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
render Home = topBarWithContent [ welcomeBanner, games ]
render (Game handle) = topBarWithContent [ game handle ]
render (Player nickname) = topBarWithContent [ player nickname ]
render Register = singleContent [ register ]
render SignIn = singleContent [ signIn ]
render (Welcome welcomeData) = singleContent [ welcome welcomeData ]
render NotFound = HH.p_ [ HH.text "You're fucken lost, mate." ]

just :: forall t5 t7. Applicative t5 => t7 -> t5 (Maybe t7)
just = pure <<< Just

nothing :: forall t1 t3. Applicative t1 => t1 (Maybe t3)
nothing = pure Nothing

handleAction :: forall action output wut left.
    Action -> H.HalogenM State action wut output (Async left) Unit
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
        ["", "games", handle] ->
            just $ Game handle
        ["", "players", nickname] ->
            just $ Player nickname
        _ ->
            just NotFound
    case newState of
        Just newState' -> H.put newState'
        Nothing -> pure unit
    pure unit

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

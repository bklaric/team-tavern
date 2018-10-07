module TeamTavern.Client.Components.TopBar (Query, Slot, topBar) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Script.Cookie (PlayerInfo, deletePlayerInfo, getPlayerInfo)
import TeamTavern.Client.Script.Navigate (navigate_)

data Query send
    = Init send
    | SignOut send

type State = Maybe PlayerInfo

type Slot = H.Slot Query Void

type ChildSlots =
    ( homeAnchor :: NavigationAnchor.Slot Unit
    , signInAnchor :: NavigationAnchor.Slot Unit
    , registerAnchor :: NavigationAnchor.Slot Unit
    , profileAnchor :: NavigationAnchor.Slot Unit
    , createGameAnchor :: NavigationAnchor.Slot Unit
    )

render :: forall monad. MonadEffect monad =>
    State -> H.ComponentHTML Query ChildSlots monad
render playerInfo = HH.div_
    [ HH.div [ HP.id_ "top-bar-filler" ] []
    , HH.div [ HP.id_ "top-bar" ]
        [ HH.div [ HP.id_ "top-bar-content" ]
            [ HH.span [ HP.id_ "top-bar-title" ]
                [ navigationAnchor (SProxy :: SProxy "homeAnchor")
                    { path: "/", text: "TeamTavern" }
                ]
            , HH.div_ case playerInfo of
                Nothing ->
                    [ navigationAnchor (SProxy :: SProxy "registerAnchor")
                        { path: "/register", text: "Register" }
                    , navigationAnchor (SProxy :: SProxy "signInAnchor")
                        { path: "/signin", text: "Sign in" }
                    ]
                Just { id, nickname } ->
                    [ navigationAnchor (SProxy :: SProxy "profileAnchor")
                        { path: "/players/" <> nickname, text: nickname }
                    , navigationAnchor (SProxy :: SProxy "createGameAnchor")
                        { path: "/games/create", text: "Create a game" }
                    , HH.button [ HE.onClick $ HE.input_ SignOut ]
                        [ HH.text "Sign out" ]
                    ]
            ]
        ]
    ]

eval :: forall monad. MonadEffect monad =>
    Query ~> H.HalogenM State Query ChildSlots Void monad
eval (Init send) = do
    playerInfo <- getPlayerInfo # H.liftEffect
    H.put playerInfo
    pure send
eval (SignOut send) = do
    deletePlayerInfo # H.liftEffect
    H.put Nothing
    navigate_ "/" # H.liftEffect
    pure send

component :: forall input m. MonadEffect m =>
    H.Component HH.HTML Query input Void m
component =
    H.component
        { initialState: const Nothing
        , render
        , eval
        , receiver: const Nothing
        , initializer: Just $ Init unit
        , finalizer: Nothing
        }

topBar :: forall query monad children. MonadEffect monad =>
    HH.ComponentHTML query (topBar :: Slot Unit | children) monad
topBar = HH.slot (SProxy :: SProxy "topBar") unit component unit absurd

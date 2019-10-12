module TeamTavern.Client.Components.TopBar (Slot, topBar) where

import Prelude

import Async (Async, fromEffect, left, unify)
import Browser.Async.Fetch (fetch, fetch_, method)
import Browser.Async.Fetch.Response (text)
import Browser.Fetch.Response (status)
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON.Async (readJSON)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.CreateGame (createGame)
import TeamTavern.Client.CreateGame as CreateGame
import TeamTavern.Client.Script.Cookie (getPlayerId)
import TeamTavern.Client.Script.Navigate (navigateWithEvent_, navigate_)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data Action
    = Init
    | SignOut
    | ShowCreateModal MouseEvent
    | HandleModalMessage (Modal.Message CreateGame.Message)
    | Navigate String MouseEvent

data State
    = Empty
    | SignedOut
    | SignedIn { nickname :: String }

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( profileAnchor :: NavigationAnchor.Slot Unit
    , createGameAnchor :: NavigationAnchor.Slot Unit
    , createGame :: CreateGame.Slot Unit
    )

render :: forall left.
    State -> H.ComponentHTML Action ChildSlots (Async left)
render playerInfo = HH.div_
    [ HH.div [ HP.class_ $ HH.ClassName "top-bar" ]
        [ HH.div [ HP.class_ $ HH.ClassName "top-bar-content" ]
            [ HH.h2 [ HP.class_ $ HH.ClassName "top-bar-title" ]
                [ HH.a
                    [ HP.class_ $ HH.ClassName "top-bar-title-link"
                    , HP.href "/"
                    , HE.onClick $ Just <<< Navigate "/"
                    ]
                    [ HH.img
                        [ HP.class_ $ HH.ClassName "top-bar-logo"
                        , HP.src "/favicon-32x32.png"
                        ]
                    , HH.text "TeamTavern"
                    ]
                ]
            , HH.div_ case playerInfo of
                Empty -> []
                SignedOut ->
                    [ HH.a
                        [ HP.class_ $ HH.ClassName "top-bar-link"
                        , HP.href "/signin"
                        , HE.onClick $ Just <<< Navigate "/signin"
                        ]
                        [ HH.text "Sign in" ]
                    , HH.a
                        [ HP.class_ $ HH.ClassName "top-bar-link"
                        , HP.href "/register"
                        , HE.onClick $ Just <<< Navigate "/register"
                        ]
                        [ HH.text "Create account" ]
                    ]
                SignedIn { nickname } ->
                    [ HH.a
                        [ HP.class_ $ HH.ClassName "top-bar-link"
                        , HP.href $ "/players/" <> nickname
                        , HE.onClick $ Just <<< Navigate ("/players/" <> nickname)
                        ]
                        [ HH.text nickname ]
                    -- , HH.a
                    --     [ HP.href ""
                    --     , HE.onClick $ Just <<< ShowCreateModal
                    --     ]
                    --     [ HH.text "Create a game" ]
                    , HH.button
                        [ HP.class_ $ HH.ClassName "top-bar-button"
                        , HE.onClick $ const $ Just SignOut
                        ]
                        [ HH.i [ HP.class_ $ H.ClassName "fas fa-sign-out-alt button-icon" ] []
                        , HH.text "Sign out"
                        ]
                    ]
            ]
        ]
    , HH.div_ case playerInfo of
        SignedIn _ -> [ createGame $ Just <<< HandleModalMessage ]
        _ -> []
    ]

getPlayerHeader :: forall left. Int -> Async left State
getPlayerHeader playerId = unify do
    response <- fetch_ ("/api/players/" <> show playerId <> "/header")
        # lmap (const Empty)
    { nickname } :: { nickname :: String } <- case status response of
        200 -> text response >>= readJSON # lmap (const Empty)
        _ -> left Empty
    pure $ SignedIn { nickname }

endSession :: forall left. Async left Boolean
endSession = unify do
    response <- fetch ("/api/sessions/current") (method := DELETE)
        # lmap (const false)
    pure $ status response == 204

handleAction :: forall left output.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Init = do
    playerId <- getPlayerId # fromEffect # H.lift
    H.put =<< case playerId of
        Nothing -> pure SignedOut
        Just id -> H.lift $ getPlayerHeader id
    pure unit
handleAction SignOut = do
    success <- endSession # H.lift
    when success do
        H.put SignedOut
        navigate_ "/" # fromEffect # H.lift
    pure unit
handleAction (ShowCreateModal event) = do
    H.liftEffect $ preventDefault $ toEvent event
    Modal.show (SProxy :: SProxy "createGame")
handleAction (HandleModalMessage _) =
    Modal.hide (SProxy :: SProxy "createGame")
handleAction (Navigate url event) =
    H.liftEffect $ navigateWithEvent_ url event

component :: forall query input output left.
    H.Component HH.HTML query input output (Async left)
component =
    H.mkComponent
        { initialState: const $ Empty
        , render
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            , initialize = Just Init
            }
        }

topBar :: forall query void children.
    HH.ComponentHTML query (topBar :: Slot Unit | children) (Async void)
topBar = HH.slot (SProxy :: SProxy "topBar") unit component unit absurd

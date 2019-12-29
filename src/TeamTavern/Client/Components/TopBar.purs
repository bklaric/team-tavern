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
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.CreateGame (createGame)
import TeamTavern.Client.CreateGame as CreateGame
import TeamTavern.Client.Script.Cookie (getPlayerId)
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Script.Unscrollable (makeWindowScrollable, makeWindowUnscrollable)
import Web.Event.Event (preventDefault)
import Web.HTML (window)
import Web.HTML.Location (setPathname)
import Web.HTML.Window (location)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type MenuVisible = Boolean

type Nickname = String

data Loaded = SignedOut | SignedIn Nickname

data State
    = Empty
    | Loaded MenuVisible Loaded

data Action
    = Init
    | SignOut
    | ShowCreateModal MouseEvent
    | HandleModalMessage (Modal.Message CreateGame.Message)
    | Navigate String (Maybe Loaded) MouseEvent
    | ToggleMenu MenuVisible Loaded
    | CloseMenu Loaded

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( profileAnchor :: NavigationAnchor.Slot Unit
    , createGameAnchor :: NavigationAnchor.Slot Unit
    , createGame :: CreateGame.Slot Unit
    )

render :: forall left.
    State -> H.ComponentHTML Action ChildSlots (Async left)
render state = HH.div_
    [ HH.div [ HP.class_ $ HH.ClassName "top-bar" ]
        [ HH.div [ HP.class_ $ HH.ClassName "top-bar-content" ]
            [ HH.span [ HP.class_ $ HH.ClassName "top-bar-title" ]
                [ HH.a
                    [ HP.class_ $ HH.ClassName "top-bar-title-link"
                    , HP.href "/"
                    , HE.onClick $ Just <<< Navigate "/"
                        case state of
                        Empty -> Nothing
                        Loaded _ loaded -> Just loaded
                    ]
                    [ HH.img
                        [ HP.class_ $ HH.ClassName "top-bar-logo"
                        , HP.src "/favicon-32x32.png"
                        ]
                    , HH.text "TeamTavern"
                    ]
                ]
            , HH.div [ HP.class_ $ HH.ClassName "top-bar-menu" ]
                case state of
                Empty -> []
                Loaded menuVisible loaded ->
                    [ HH.button
                        [ HP.class_ $ HH.ClassName "top-bar-menu-button"
                        , HE.onClick $ const $ Just $ ToggleMenu menuVisible loaded
                        ]
                        [ HH.i [ HP.class_ $ HH.ClassName if menuVisible then "fas fa-times top-bar-menu-button-icon" else "fas fa-bars top-bar-menu-button-icon" ] [] ]
                    , HH.div
                        [ HP.class_ $ HH.ClassName if menuVisible then "top-bar-menu-items" else "hidden-top-bar-menu-items"
                        , HE.onClick $ const $ Just $ CloseMenu loaded
                        ]
                        case loaded of
                        SignedOut ->
                            [ HH.a
                                [ HP.class_ $ HH.ClassName "top-bar-menu-item"
                                , HP.href "/signin"
                                , HE.onClick $ Just <<< Navigate "/signin" Nothing
                                ]
                                [ HH.text "Sign in" ]
                            , HH.a
                                [ HP.class_ $ HH.ClassName "top-bar-menu-item"
                                , HP.href "/register"
                                , HE.onClick $ Just <<< Navigate "/register" Nothing
                                ]
                                [ HH.text "Create account" ]
                            ]
                        SignedIn nickname ->
                            [ HH.a
                                [ HP.class_ $ HH.ClassName "top-bar-menu-item"
                                , HP.href $ "/account"
                                , HE.onClick $ Just <<< Navigate "/account" Nothing
                                ]
                                [ HH.text "Account" ]
                            , HH.button
                                [ HP.class_ $ HH.ClassName "top-bar-menu-item top-bar-button"
                                , HE.onClick $ const $ Just $ SignOut
                                ]
                                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-sign-out-alt button-icon" ] []
                                , HH.text "Sign out"
                                ]
                            ]
                    ]
            ]
        ]
    , HH.div [ HP.class_ $ HH.ClassName "top-bar-filler" ] []
    , HH.div_ case state of
        Loaded _ (SignedIn _) -> [ createGame $ Just <<< HandleModalMessage ]
        _ -> []
    ]

getPlayerHeader :: forall left. Int -> Async left State
getPlayerHeader playerId = unify do
    response <- fetch_ ("/api/players/" <> show playerId <> "/header")
        # lmap (const Empty)
    { nickname } :: { nickname :: String } <- case status response of
        200 -> text response >>= readJSON # lmap (const Empty)
        _ -> left Empty
    pure $ Loaded false $ SignedIn nickname

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
        Nothing -> pure $ Loaded false SignedOut
        Just id -> H.lift $ getPlayerHeader id
handleAction SignOut = do
    success <- H.lift endSession
    when success do
        window >>= location >>= setPathname "/" # H.liftEffect
handleAction (ShowCreateModal event) = do
    H.liftEffect $ preventDefault $ toEvent event
    Modal.show (SProxy :: SProxy "createGame")
handleAction (HandleModalMessage _) =
    Modal.hide (SProxy :: SProxy "createGame")
handleAction (Navigate url loaded event) = do
    H.liftEffect $ navigateWithEvent_ url event
    case loaded of
        Nothing -> pure unit
        Just loaded' -> H.put $ Loaded false loaded'
handleAction (ToggleMenu menuVisible loaded) = do
    let nextMenuVisible = not menuVisible
    if nextMenuVisible
        then makeWindowUnscrollable
        else makeWindowScrollable
    H.put $ Loaded nextMenuVisible loaded
handleAction (CloseMenu loaded) =
    H.put $ Loaded false loaded

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

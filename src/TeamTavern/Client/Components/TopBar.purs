module TeamTavern.Client.Components.TopBar (Slot, topBar) where

import Prelude

import Async (Async, unify)
import Async as Async
import Async.Aff (affToAsync)
import Browser.Async.Fetch (fetch, method)
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Browser.Fetch.Response (status)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Options ((:=))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Script.Cookie (getPlayerNickname)
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Routes.ViewAllGames as ViewAllGames
import Web.Event.Event (stopPropagation)
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.Location (setPathname)
import Web.HTML.Window (location)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

type Games = ViewAllGames.OkContent

type GamesVisible = Boolean

type MenuVisible = Boolean

type Nickname = String

data PlayerStatus = Unknown | SignedOut | SignedIn Nickname

type State =
    { windowSubscription :: Maybe H.SubscriptionId
    , games :: Maybe Games
    , gamesVisible :: Boolean
    , menuVisible :: Boolean
    , playerStatus :: PlayerStatus
    }

data Action
    = Initialize
    | Finalize
    | SignOut
    | Navigate String MouseEvent
    | ToggleMenu
    | CloseMenu
    | ToggleGamesPopunder MouseEvent
    | CloseGamesPopunder

type Slot = H.Slot (Const Void) Void

render :: forall children left.
    State -> H.ComponentHTML Action children (Async left)
render state = HH.div_ $
    [ HH.div [ HP.class_ $ HH.ClassName "top-bar" ]
        [ HH.div [ HP.class_ $ HH.ClassName "top-bar-content" ]
            [ HH.div [ HP.class_ $ HH.ClassName "top-bar-left" ]
                [ HH.a
                    [ HP.class_ $ HH.ClassName "top-bar-title top-bar-title-link"
                    , HP.href "/"
                    , HE.onClick $ Just <<< Navigate "/"
                    ]
                    [ HH.img
                        [ HP.class_ $ HH.ClassName "top-bar-logo"
                        , HP.src "/favicons/favicon-32x32.png"
                        , HP.alt "TeamTavern logo"
                        ]
                    , HH.text "TeamTavern"
                    ]
                , HH.a
                    [ HP.class_ $ HH.ClassName "top-bar-games"
                    , HP.href "/games"
                    , HE.onClick $ Just <<< Navigate "/games"
                    ]
                    [ HH.text "Games"]
                , HH.div [ HP.class_ $ HH.ClassName "popover-container" ] $
                    [ HH.i
                        [ HP.class_ $ HH.ClassName "fas fa-caret-down top-bar-games-caret"
                        , HE.onClick $ Just <<< ToggleGamesPopunder
                        ]
                        []
                    ]
                    <>
                    if state.gamesVisible
                    then Array.singleton $
                        HH.div [ HP.class_ $ HH.ClassName "top-bar-games-popover" ]
                        case state of
                        { games: Just games } -> games <#> \game ->
                            HH.div [ HP.class_ $ HH.ClassName "top-bar-game" ]
                            [ HH.a
                                [ HP.class_ $ HH.ClassName "top-bar-game-link"
                                , HP.href $ "/games/" <> game.handle
                                , HE.onClick $ Just <<< Navigate ("/games/" <> game.handle)
                                ]
                                [ HH.img
                                    [ HP.class_ $ HH.ClassName "top-bar-game-icon"
                                    , HP.src $ "/images/" <> game.handle <> "/icon-orange.png"
                                    ]
                                , HH.span [ HP.class_ $ HH.ClassName "top-bar-game-title" ]
                                    [ HH.text game.title ]
                                ]
                            , HH.div_
                                [ HH.a
                                    [ HP.href $ "/games/" <> game.handle <> "/players"
                                    , HE.onClick $ Just <<< Navigate ("/games/" <> game.handle <> "/players")
                                    ]
                                    [ HH.text "Players" ]
                                , divider
                                , HH.a
                                    [ HP.href $ "/games/" <> game.handle <> "/teams"
                                    , HE.onClick $ Just <<< Navigate ("/games/" <> game.handle <> "/teams")
                                    ]
                                    [ HH.text "Teams" ]
                                ]
                            ]
                        _ -> []
                    else []
                ]
            , HH.div [ HP.class_ $ HH.ClassName "top-bar-menu" ]
                [ HH.button
                    [ HP.class_ $ HH.ClassName "top-bar-menu-button"
                    , HE.onClick $ const $ Just ToggleMenu
                    ]
                    [ HH.i [ HP.class_ $ HH.ClassName if state.menuVisible then "fas fa-times top-bar-menu-button-icon" else "fas fa-bars top-bar-menu-button-icon" ] [] ]
                , HH.div
                    [ HP.class_ $ HH.ClassName if state.menuVisible then "top-bar-menu-items" else "hidden-top-bar-menu-items"
                    , HE.onClick $ const $ Just CloseMenu
                    ]
                    case state.playerStatus of
                    Unknown -> []
                    SignedOut ->
                        [ HH.a
                            [ HP.class_ $ HH.ClassName "top-bar-menu-item"
                            , HP.href "/signin"
                            , HE.onClick $ Just <<< Navigate "/signin"
                            ]
                            [ HH.text "Sign in" ]
                        , HH.a
                            [ HP.class_ $ HH.ClassName "top-bar-menu-item"
                            , HP.href "/register"
                            , HE.onClick $ Just <<< Navigate "/register"
                            ]
                            [ HH.text "Create account" ]
                        ]
                    SignedIn nickname ->
                        [ HH.a
                            [ HP.class_ $ HH.ClassName "top-bar-menu-item"
                            , HP.href $ "/players/" <> nickname
                            , HE.onClick $ Just <<< Navigate ("/players/" <> nickname)
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
    ]

endSession :: forall left. Async left Boolean
endSession = unify do
    response <- fetch ("/api/sessions/current") (method := DELETE)
        # lmap (const false)
    pure $ status response == 204

loadGames :: forall left. Async left (Maybe Games)
loadGames = Async.unify do
    response <- Fetch.fetch_ "/api/games" # lmap (const Nothing)
    games :: Games <-
        case FetchRes.status response of
        200 -> FetchRes.text response
            >>= JsonAsync.readJSON
            # lmap (const Nothing)
        _ -> Async.left Nothing
    pure $ Just games

handleAction :: forall children left output.
    Action -> H.HalogenM State Action children output (Async left) Unit
handleAction Initialize = do
    nickname <- getPlayerNickname

    window <- H.liftEffect $ Window.toEventTarget <$> window
    let windowEventSource = ES.eventListenerEventSource
            (E.EventType "click") window \_ -> Just CloseGamesPopunder
    windowSubscription <- H.subscribe $ ES.hoist affToAsync windowEventSource

    games <- H.lift loadGames

    H.put
        { windowSubscription: Just windowSubscription
        , games
        , gamesVisible: false
        , menuVisible: false
        , playerStatus: maybe SignedOut SignedIn nickname
        }
handleAction Finalize = do
    { windowSubscription } <- H.get
    case windowSubscription of
        Just windowSubscription' -> H.unsubscribe windowSubscription'
        Nothing -> pure unit
handleAction SignOut = do
    success <- H.lift endSession
    when success do
        window >>= location >>= setPathname "/" # H.liftEffect
handleAction (Navigate url event) = do
    navigateWithEvent_ url event
    H.modify_ (_ { gamesVisible = false, menuVisible = false })
handleAction ToggleMenu=
    H.modify_ \state -> state { menuVisible = not state.menuVisible }
handleAction CloseMenu =
    H.modify_ (_ { menuVisible = false })
handleAction (ToggleGamesPopunder mouseEvent) = do
    H.liftEffect $ stopPropagation $ MouseEvent.toEvent mouseEvent
    H.modify_ \state -> state { gamesVisible = not state.gamesVisible }
handleAction CloseGamesPopunder =
    H.modify_ (_ { gamesVisible = false })

component :: forall query input output left.
    H.Component HH.HTML query input output (Async left)
component =
    H.mkComponent
        { initialState: const
            { windowSubscription: Nothing
            , games: Nothing
            , gamesVisible: false
            , menuVisible: false
            , playerStatus: Unknown
            }
        , render
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
        }

topBar :: forall query void children.
    HH.ComponentHTML query (topBar :: Slot Unit | children) (Async void)
topBar = HH.slot (SProxy :: SProxy "topBar") unit component unit absurd

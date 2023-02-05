module TeamTavern.Client.Components.TopBar (topBar) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array (find)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as ES
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Script.Cookie (getPlayerNickname)
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Script.Request (get)
import TeamTavern.Client.Shared.Fetch (fetchSimple)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Game.ViewAllGames as ViewAllGames
import TeamTavern.Routes.Session.EndSession (EndSession)
import Type.Proxy (Proxy(..))
import Web.Event.Event (stopPropagation)
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.Location (setPathname)
import Web.HTML.Window (location)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

type Game = ViewAllGames.OkGameContent

type Games = ViewAllGames.OkContent

type Input = Maybe String

type Nickname = String

data PlayerStatus = Unknown | SignedOut | SignedIn Nickname

type State =
    { handle :: Maybe String
    , games :: Maybe Games
    , selectedGame :: Maybe Game
    , gamesVisible :: Boolean
    , windowSubscription :: Maybe H.SubscriptionId
    , menuVisible :: Boolean
    , playerStatus :: PlayerStatus
    }

data Action
    = Initialize
    | Receive Input
    | Finalize
    | SignOut
    | Navigate String MouseEvent
    | ToggleMenu
    | CloseMenu
    | ToggleGamesPopunder MouseEvent
    | CloseGamesPopunder

render :: ∀ left. State -> H.ComponentHTML Action _ (Async left)
render state = HH.div_ $
    [ HH.div [ HS.class_ "top-bar" ]
        [ HH.div [ HS.class_ "top-bar-content" ]
            [ HH.div [ HS.class_ "top-bar-left" ] $
                [ HH.a
                    [ HS.class_ "top-bar-title"
                    , HP.href "/"
                    , HE.onClick $ Navigate "/"
                    ]
                    [ HH.img
                        [ HS.class_ "top-bar-title-logo"
                        , HP.src "/favicons/apple-touch-icon.png"
                        , HP.alt "TeamTavern logo"
                        ]
                    , HH.span [ HS.class_ "top-bar-title-text" ] [ HH.text "TeamTavern" ]
                    ]
                , HH.div [ HS.class_ "top-bar-game-selection" ]
                    [ case state.selectedGame of
                        Nothing ->
                            HH.a
                            [ HS.class_ "top-bar-games"
                            , HP.href "/games"
                            , HE.onClick $ Navigate "/games"
                            ]
                            [ HH.i [ HS.class_ "fas fa-gamepad top-bar-games-icon"] []
                            , HH.text "Games"
                            ]
                        Just { handle, title, shortTitle } ->
                            HH.a
                            [ HS.class_ "top-bar-games"
                            , HP.href $ "/games/" <> handle
                            , HE.onClick $ Navigate ("/games/" <> handle)
                            ]
                            [ HH.img
                                [ HS.class_ "top-bar-games-icon"
                                , HP.src $ "/images/" <> handle <> "/icon-orange.png"
                                ]
                            , HH.span [ HS.class_ "top-bar-games-title" ] [ HH.text title ]
                            , HH.span [ HS.class_ "top-bar-games-short-title" ] [ HH.text shortTitle ]
                            ]
                    , HH.div [ HS.class_ "top-bar-games-popover-container" ] $
                        [ HH.i
                            [ HS.class_ "fas fa-caret-down top-bar-games-caret"
                            , HE.onClick $ ToggleGamesPopunder
                            ]
                            []
                        ]
                        <>
                        ( guard state.gamesVisible $ Array.singleton $
                            HH.div [ HS.class_ "top-bar-games-popover" ] $
                            foldMap (_ <#> \{ handle, title } ->
                                HH.div [ HS.class_ "top-bar-game" ]
                                [ HH.a
                                    [ HS.class_ "top-bar-game-link"
                                    , HP.href $ "/games/" <> handle
                                    , HE.onClick $ Navigate ("/games/" <> handle)
                                    ]
                                    [ HH.img
                                        [ HS.class_ "top-bar-game-icon"
                                        , HP.src $ "/images/" <> handle <> "/icon-orange.png"
                                        ]
                                    , HH.span [ HS.class_ "top-bar-game-title" ]
                                        [ HH.text title ]
                                    ]
                                , HH.div_
                                    [ HH.a
                                        [ HP.href $ "/games/" <> handle <> "/players"
                                        , HE.onClick $ Navigate ("/games/" <> handle <> "/players")
                                        ]
                                        [ HH.text "Players" ]
                                    , divider
                                    , HH.a
                                        [ HP.href $ "/games/" <> handle <> "/teams"
                                        , HE.onClick $ Navigate ("/games/" <> handle <> "/teams")
                                        ]
                                        [ HH.text "Teams" ]
                                    ]
                                ])
                                state.games
                        )
                    ]
                ]
                <>
                foldMap (\{ handle } ->
                    [ HH.div [ HS.class_ "top-bar-navigation" ]
                        [ HH.a
                            [ HS.class_ "top-bar-navigation-item"
                            , HP.href $ "/games/" <> handle <> "/players"
                            , HE.onClick $ Navigate ("/games/" <> handle <> "/players")
                            ]
                            [ HH.i [ HS.class_ "fas fa-user top-bar-navigation-item-icon" ] []
                            , HH.span [ HS.class_ "top-bar-navigation-item-text" ] [ HH.text "Players" ]
                            ]
                        , HH.a
                            [ HS.class_ "top-bar-navigation-item"
                            , HP.href $ "/games/" <> handle <> "/teams"
                            , HE.onClick $ Navigate ("/games/" <> handle <> "/teams")
                            ]
                            [ HH.i [ HS.class_ "fas fa-users top-bar-navigation-item-icon" ] []
                            , HH.span [ HS.class_ "top-bar-navigation-item-text" ] [ HH.text "Teams" ]
                            ]
                        ]
                    ])
                    state.selectedGame
            , HH.div [ HS.class_ "top-bar-menu" ]
                [ HH.button
                    [ HS.class_ "top-bar-menu-button"
                    , HP.title "Menu"
                    , HE.onClick $ const ToggleMenu
                    ]
                    [ HH.i [ HS.class_ if state.menuVisible then "fas fa-times top-bar-menu-button-icon" else "fas fa-bars top-bar-menu-button-icon" ] [] ]
                , HH.div
                    [ HS.class_ if state.menuVisible then "top-bar-menu-items" else "hidden-top-bar-menu-items"
                    , HE.onClick $ const CloseMenu
                    ]
                    case state.playerStatus of
                    Unknown -> []
                    SignedOut ->
                        [ HH.a
                            [ HS.class_ "top-bar-menu-item"
                            , HP.href "/signin"
                            , HE.onClick $ Navigate "/signin"
                            ]
                            [ HH.text "Sign in" ]
                        , HH.a
                            [ HS.class_ "top-bar-menu-item"
                            , HP.href "/register"
                            , HE.onClick $ Navigate "/register"
                            ]
                            [ HH.text "Create account" ]
                        ]
                    SignedIn nickname ->
                        [ HH.a
                            [ HS.class_ "top-bar-menu-item"
                            , HP.href $ "/players/" <> nickname
                            , HE.onClick $ Navigate ("/players/" <> nickname)
                            ]
                            [ HH.text "Account" ]
                        , HH.button
                            [ HS.class_ "top-bar-menu-item top-bar-button"
                            , HE.onClick $ const SignOut
                            ]
                            [ HH.i [ HS.class_ "fas fa-sign-out-alt button-icon" ] []
                            , HH.text "Sign out"
                            ]
                        ]
                ]
            ]
        ]
    , HH.div [ HS.class_ "top-bar-filler" ] []
    ]

endSession :: ∀ left. Async left Boolean
endSession = Async.unify do
    fetchSimple (Proxy :: _ EndSession) # bimap (const false) (const true)

handleAction :: ∀ children left output.
    Action -> H.HalogenM State Action children output (Async left) Unit
handleAction Initialize = do
    nickname <- getPlayerNickname

    window <- H.liftEffect $ Window.toEventTarget <$> window
    let windowEventSource = ES.eventListener
            (E.EventType "click") window \_ -> Just CloseGamesPopunder
    windowSubscription <- H.subscribe windowEventSource

    games' <- H.lift $ get "/api/games"

    H.modify_ \state -> state
        { windowSubscription = Just windowSubscription
        , games = games'
        , selectedGame =
            case state.handle, games' of
            Just handle, Just games -> find (_.handle >>> eq handle) games
            _, _ -> Nothing
        , playerStatus = maybe SignedOut SignedIn nickname
        }
handleAction (Receive handle') =
    H.modify_ \state -> state
        { handle = handle'
        , selectedGame =
            case handle', state.games of
            Just handle, Just games -> find (_.handle >>> eq handle) games
            _, _ -> Nothing
        }
handleAction Finalize = do
    { windowSubscription } <- H.get
    foldMap H.unsubscribe windowSubscription
handleAction SignOut = do
    success <- H.lift endSession
    when success do window >>= location >>= setPathname "/" # H.liftEffect
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

component :: ∀ query output left. H.Component query Input output (Async left)
component = H.mkComponent
    { initialState:
        { handle: _
        , games: Nothing
        , selectedGame: Nothing
        , gamesVisible: false
        , windowSubscription: Nothing
        , menuVisible: false
        , playerStatus: Unknown
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

topBar :: ∀ action left slots.
    Input -> HH.ComponentHTML action (topBar :: Slot___ | slots) (Async left)
topBar input = HH.slot (Proxy :: _ "topBar") unit component input absurd

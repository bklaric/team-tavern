module TeamTavern.Client.Components.TopBar (Query, Slot, topBar) where

import Prelude

import Async (Async, fromEffect, left, unify)
import Browser.Async.Fetch (fetch_)
import Browser.Async.Fetch.Response (text)
import Browser.Fetch.Response (status)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON.Async (readJSON)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Script.Cookie (deletePlayerInfo, getPlayerId)
import TeamTavern.Client.Script.Navigate (navigate_)

data Query send
    = Init send
    | SignOut send

data State
    = Empty
    | Header (Maybe { nickname :: String })
    | Error

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
                Empty -> []
                Header Nothing ->
                    [ navigationAnchor (SProxy :: SProxy "signInAnchor")
                        { path: "/signin", text: "Sign in" }
                    , navigationAnchor (SProxy :: SProxy "registerAnchor")
                        { path: "/register", text: "Register" }
                    ]
                Header (Just { nickname }) ->
                    [ navigationAnchor (SProxy :: SProxy "profileAnchor")
                        { path: "/players/" <> nickname, text: nickname }
                    , navigationAnchor (SProxy :: SProxy "createGameAnchor")
                        { path: "/games/create", text: "Create a game" }
                    , HH.button [ HE.onClick $ HE.input_ SignOut ]
                        [ HH.text "Sign out" ]
                    ]
                Error ->
                    [ HH.button [ HE.onClick $ HE.input_ SignOut ]
                        [ HH.text "Sign out" ]
                    ]
            ]
        ]
    ]

getPlayerHeader :: forall left. Int -> Async left State
getPlayerHeader playerId = unify do
    response <- fetch_ ("/api/players/" <> show playerId <> "/header")
        # lmap (const Error)
    content <- case status response of
        200 -> text response >>= readJSON # lmap (const Error)
        _ -> left Error
    pure $ Header $ Just content

eval :: forall left. Query ~> H.HalogenM State Query ChildSlots Void (Async left)
eval (Init send) = do
    playerId <- getPlayerId # fromEffect # H.lift
    H.put =<< case playerId of
        Nothing -> pure $ Header Nothing
        Just id -> H.lift $ getPlayerHeader id
    pure send
eval (SignOut send) = do
    deletePlayerInfo # fromEffect # H.lift
    H.put $ Header Nothing
    navigate_ "/" # fromEffect # H.lift
    pure send

component :: forall input void.
    H.Component HH.HTML Query input Void (Async void)
component =
    H.component
        { initialState: const $ Header Nothing
        , render
        , eval
        , receiver: const Nothing
        , initializer: Just $ Init unit
        , finalizer: Nothing
        }

topBar :: forall query void children.
    HH.ComponentHTML query (topBar :: Slot Unit | children) (Async void)
topBar = HH.slot (SProxy :: SProxy "topBar") unit component unit absurd

module TeamTavern.Client.Game (Query, State', State, Slot, game) where

import Prelude

import Async (Async(..))
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Control.Plus (empty)
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Simple.JSON as Json
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Game.View.Response (OkContent)
import TeamTavern.Game.View.Response as View
import TeamTavern.Profile.ViewByGame.Response as ViewByGame

data Query send = Init send

data State'
    = Game (Maybe { game :: View.OkContent, profiles :: ViewByGame.OkContent })
    | Error

type State = {
    handle :: String,
    state :: State'
}

type Slot = H.Slot Query Void

type ChildSlots = (players :: Anchor.Slot Int)

render :: forall left. State -> H.ComponentHTML Query ChildSlots (Async left)
render ({ state: Game Nothing}) = HH.div_ []
render ({ state: Game (Just { game: { name, description }, profiles }) }) = HH.div_
    [ HH.h2_ [ HH.text name ]
    , HH.p_ [ HH.text description ]
    , HH.ul_ $
        profiles # mapWithIndex \index { nickname, summary } -> HH.li_
            [ HH.h3_ [ HH.slot (SProxy :: SProxy "players") index navigationAnchor { path: "/players/" <> nickname, text: nickname } absurd ]
            , HH.p_ [ HH.text summary ]
            ]
    ]
render { state: Error } = HH.h2_ [ HH.text "There has been an error loading the game." ]

eval :: forall left. Query ~> H.HalogenM State Query ChildSlots Void (Async left)
eval (Init send) = do
    { handle } <- H.get
    responseGame' <- H.lift $ Async.attempt $ Fetch.fetch
        ("/api/games/" <> handle )
        (Fetch.method := GET)
    stateWithGame <- case responseGame' of
        Right responseGame -> H.lift
            case FetchRes.status responseGame of
            200 -> FetchRes.text responseGame <#> Json.readJSON >>=
                case _ of
                Right (content :: View.OkContent) -> pure $ Game $ Just
                    { game: content, profiles: [] }
                _ -> pure Error
            _ -> pure Error
        _ -> pure Error
    responseProfiles' <- H.lift $ Async.attempt $ Fetch.fetch
        ("/api/games/" <> handle <> "/profiles")
        (Fetch.method := GET)
    newState <- case responseProfiles' of
        Right responseProfiles -> H.lift
            case FetchRes.status responseProfiles of
            200 -> FetchRes.text responseProfiles <#> Json.readJSON >>=
                case _ of
                Left errors -> pure Error
                Right (content :: ViewByGame.OkContent) ->
                    case stateWithGame of
                    Game (Just state) -> pure $ Game $ Just
                        state { profiles = content }
                    _ -> pure Error
            _ -> pure Error
        _ -> pure Error
    H.put { state: newState, handle }
    pure send

component :: forall left. H.Component HH.HTML Query State Void (Async left)
component =
    H.component
        { initialState: identity
        , render
        , eval
        , receiver: const Nothing
        , initializer: Just $ Init unit
        , finalizer: Nothing
        }

game label handle = HH.slot label unit component ({ state: Game Nothing, handle }) absurd

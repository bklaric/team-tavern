module TeamTavern.Client.Components.Games (Slot, games) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (length)
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Server.Game.ViewAll.SendResponse (OkContent)

data Action = Init

data State = Empty | Games OkContent

type Slot = H.Slot (Const Void) Void

type ChildSlots = (game :: Anchor.Slot Int)

render :: forall query monad. MonadEffect monad =>
    State -> H.ComponentHTML query ChildSlots monad
render Empty = HH.div_ []
render (Games games') =
    if length games' > 0
    then
        HH.div_ $
        games' # mapWithIndex \index { title, handle, description, profileCount } ->
            HH.div [ HP.class_ $ ClassName "card" ] $
            [ HH.h2_
                [ navigationAnchorIndexed (SProxy :: SProxy "game") index
                    { path: "/games/" <> handle, content: HH.text title }
                , HH.span [ HP.class_ $ ClassName "divider" ]
                    [ HH.text "•"]
                , navigationAnchorIndexed (SProxy :: SProxy "game") index
                    { path: "/games/" <> handle
                    , content: HH.span [ HP.class_ $ ClassName "profile-count" ]
                        [ HH.text $ show profileCount <> " profiles" ]
                    }
                ]
            ] <> (description <#> \paragraph -> HH.p_ [ HH.text paragraph ])
    else HH.p_ [ HH.text $
        "There should be a list of games here, "
        <> "but no game entry has been created yet. "
        <> "How about you create one?" ]

loadGames :: forall left. Async left State
loadGames = Async.unify do
    response' <- Fetch.fetch_ "/api/games" # lmap (const Empty)
    games' :: OkContent <-
        case FetchRes.status response' of
        200 -> FetchRes.text response'
            >>= JsonAsync.readJSON
            # lmap (const Empty)
        _ -> Async.left Empty
    pure $ Games games'

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Init = do
    newState <- H.lift loadGames
    H.put newState

component :: forall query output left.
    H.Component HH.HTML query State output (Async left)
component =
    H.mkComponent
        { initialState: identity
        , render
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            , initialize = Just Init
            }
        }

games :: forall query children left.
    HH.ComponentHTML query (games :: Slot Unit | children) (Async left)
games = HH.slot (SProxy :: SProxy "games") unit component Empty absurd

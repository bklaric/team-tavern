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
import TeamTavern.Game.ViewAll.SendResponse (OkContent)

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
        games' # mapWithIndex \index { title, handle, description } ->
            HH.div [ HP.class_ $ ClassName "game-item" ]
            [ HH.h2_ [ navigationAnchorIndexed (SProxy :: SProxy "game") index
                { path: "/games/" <> handle, text: title } ]
            , HH.p_ [ HH.text description ]
            ]
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

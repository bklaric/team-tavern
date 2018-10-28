module TeamTavern.Client.Blocks.Games (Query, Slot, games) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Game.ViewAll.Response (OkContent)

data Query send = Init send

data State
    = Games OkContent
    | Error

type Slot = H.Slot Query Void

type ChildSlots = (game :: Anchor.Slot Int)

render :: forall left. State -> H.ComponentHTML Query ChildSlots (Async left)
render (Games games') = HH.div_ $
    games' # mapWithIndex \index { title, handle, description } ->
        HH.div [ HP.class_ $ ClassName "game-item" ]
        [ HH.h2_ [ navigationAnchorIndexed (SProxy :: SProxy "game") index
            { path: "/games/" <> handle, text: title } ]
        , HH.p_ [ HH.text description ]
        ]
render Error = HH.p_ [ HH.text
    "There has been an error loading the games. Please try again later." ]

loadGames :: forall left. Async left State
loadGames = Async.unify do
    response' <- Fetch.fetch_ "/api/games" # lmap (const Error)
    games' :: OkContent <-
        case FetchRes.status response' of
        200 -> FetchRes.text response'
            >>= JsonAsync.readJSON
            # lmap (const Error)
        _ -> Async.left Error
    pure $ Games games'

eval :: forall left.
    Query ~> H.HalogenM State Query ChildSlots Void (Async left)
eval (Init send) = do
    newState <- H.lift loadGames
    H.put newState
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

games :: forall query children left.
    HH.ComponentHTML query (games :: Slot Unit | children) (Async left)
games = HH.slot (SProxy :: SProxy "games") unit component (Games []) absurd

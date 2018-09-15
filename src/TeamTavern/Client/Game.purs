module TeamTavern.Client.Game (Query, Slot, game) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Game.View.Response as View

data Query send = Init send

data State'
    = Empty
    | Game View.OkContent
    | NotFound
    | Error

type State =
    { handle :: String
    , state :: State'
    }

type Slot = H.Slot Query Void

type ChildSlots = (profiles :: Anchor.Slot Int)

render :: forall left. State -> H.ComponentHTML Query ChildSlots (Async left)
render { state: Empty } = HH.div_ []
render { state: Game { name, description } } = HH.div_
    [ HH.h2_ [ HH.text name ]
    , HH.p_ [ HH.text description ]
    ]
render { state: NotFound, handle } = HH.p_
    [ HH.text $ "Game " <> handle <> " could not be found." ]
render { state: Error, handle } = HH.p_
    [ HH.text $ "There has been an error loading game " <> handle <> "." ]

loadGame :: forall left. String -> Async left State'
loadGame handle = Async.unify do
    response <- Fetch.fetch_ ("/api/games/" <> handle) # lmap (const Error)
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Error)
        _ -> Async.left Error
    pure $ Game content

eval :: forall left.
    Query ~> H.HalogenM State Query ChildSlots Void (Async left)
eval (Init send) = do
    { handle } <- H.get
    state <- H.lift $ loadGame handle
    H.put { handle, state }
    pure send

component :: forall left. H.Component HH.HTML Query String Void (Async left)
component =
    H.component
        { initialState: { handle: _, state: Empty }
        , render
        , eval
        , receiver: const Nothing
        , initializer: Just $ Init unit
        , finalizer: Nothing
        }

game :: forall query children left.
    String -> HH.ComponentHTML query (game :: Slot Unit | children) (Async left)
game handle = HH.slot (SProxy :: SProxy "game") unit component handle absurd

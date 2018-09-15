module TeamTavern.Client.Player (Query, Slot, player) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Player.View.Response as View

data Query send = Init send

data State'
    = Empty
    | Player View.OkContent
    | NotFound
    | Error

type State =
    { nickname :: String
    , state :: State'
    }

type Slot = H.Slot Query Void

type ChildSlots = ( games :: Anchor.Slot Int )

render :: forall monad. MonadEffect monad =>
    State -> H.ComponentHTML Query ChildSlots monad
render { state: Empty }= HH.div_ []
render { state: Player { nickname, about, profiles } } = HH.div_
    [ HH.h2_ [ HH.text nickname ]
    , HH.p_ [ HH.text about ]
    , HH.ul_ $
        profiles # mapWithIndex \index { handle, name, summary } -> HH.li_
            [ HH.h3_ [ navigationAnchorIndexed (SProxy :: SProxy "games") index
                { path: "/games/" <> handle, text: name } ]
            , HH.p_ [ HH.text summary ]
            ]
    ]
render { state: NotFound, nickname } = HH.p_
    [ HH.text $ "Player " <> nickname <> " could not be found." ]
render { state: Error, nickname } = HH.p_
    [ HH.text $ "Error loading player " <> nickname <> "." ]

loadPlayer :: forall left. String -> Async left State'
loadPlayer nickname = Async.unify do
    response <- Fetch.fetch_ ("/api/players/" <> nickname) # lmap (const Error)
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Error)
        404 -> Async.left NotFound
        _ -> Async.left Error
    pure $ Player content

eval :: forall void.
    Query ~> H.HalogenM State Query ChildSlots Void (Async void)
eval (Init send) = do
    { nickname } <- H.get
    state <- H.lift $ loadPlayer nickname
    H.put { nickname, state }
    pure send

component :: forall void. H.Component HH.HTML Query String Void (Async void)
component = H.component
    { initialState: { nickname: _, state: Empty }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Init
    , finalizer: Nothing
    }

player
    :: forall query children left
    .  String
    -> HH.ComponentHTML query (player :: Slot Unit | children) (Async left)
player nickname =
    HH.slot (SProxy :: SProxy "player") unit component nickname absurd

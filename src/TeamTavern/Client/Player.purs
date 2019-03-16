module TeamTavern.Client.Player (Slot, player) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Script.Cookie (getPlayerId)
import TeamTavern.Player.View.SendResponse as View

data Action = Init String

data State
    = Empty
    | Player View.OkContent Boolean
    | NotFound
    | Error

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( edit :: Anchor.Slot Unit
    , games :: Anchor.Slot Int
    , editProfiles :: Anchor.Slot Int
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render Empty = HH.div_ []
render (Player { nickname, about } isCurrentUser) = HH.div_ $
    [ HH.div [ HP.id_ "player" ] $ join
        [ pure $ HH.h2_ [ HH.text nickname ]
        , guard isCurrentUser $ pure $ HH.p_ [
            navigationAnchor (SProxy :: SProxy "edit")
            { path: "/players/" <> nickname <> "/edit", text: "Edit info" } ]
        , pure $ HH.p_ [ HH.text about ] ]
    ]
render NotFound = HH.p_ [ HH.text "Player could not be found." ]
render Error = HH.p_ [ HH.text
    "There has been an error loading the player. Please try again later." ]

loadPlayer :: forall left. String -> Async left State
loadPlayer nickname = Async.unify do
    response <- Fetch.fetch_ ("/api/players/by-nickname/" <> nickname) # lmap (const Error)
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Error)
        404 -> Async.left NotFound
        _ -> Async.left Error
    playerId <- Async.fromEffect getPlayerId
    pure $ Player content (maybe false (_ == content.id) playerId)

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (Init nickname) = do
    state <- H.lift $ loadPlayer nickname
    H.put state
    pure unit

component :: forall query output left.
    String -> H.Component HH.HTML query String output (Async left)
component nickname = H.mkComponent
    { initialState: const Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just $ Init nickname
        , receive = Just <<< Init
        }
    }

player
    :: forall query children left
    .  String
    -> HH.ComponentHTML query (player :: Slot Unit | children) (Async left)
player nickname = HH.slot
    (SProxy :: SProxy "player") unit (component nickname) nickname absurd

module TeamTavern.Client.Game (Slot, game) where

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
import TeamTavern.Game.View.SendResponse as View

data Action = Init String

data State
    = Empty
    | Game View.OkContent Boolean Boolean
    | NotFound
    | Error

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( profiles :: Anchor.Slot Int
    , createProfile :: Anchor.Slot Unit
    , edit :: Anchor.Slot Unit
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render Empty = HH.div_ []
render (Game { title, handle, description, hasProfile } isSignedIn isAdmin) =
    HH.div [ HP.id_ "game"] $ join
    [ pure $ HH.h2_ [ HH.text title ]
    , guard (not hasProfile && isSignedIn) $ pure $ HH.p_ [
        navigationAnchor (SProxy :: SProxy "createProfile")
        { path: "/games/" <> handle <> "/profiles/create"
        , text: "Create profile"
        } ]
    , guard isAdmin $ pure $ HH.p_ [
        navigationAnchor (SProxy :: SProxy "edit")
        { path: "/games/" <> handle <> "/edit", text: "Edit game" } ]
    , pure $ HH.p_ [ HH.text description ]
    ]
render NotFound = HH.p_ [ HH.text "Game could not be found." ]
render Error = HH.p_ [ HH.text
    "There has been an error loading the game. Please try again later." ]

loadGame :: forall left. String -> Async left State
loadGame handle = Async.unify do
    response <- Fetch.fetch_ ("/api/games/by-handle/" <> handle) # lmap (const Error)
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Error)
        404 -> Async.left NotFound
        _ -> Async.left Error
    playerId <- Async.fromEffect getPlayerId
    pure $ Game content
        (maybe false (const true) playerId)
        (maybe false (_ == content.administratorId) playerId)

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (Init handle) = do
    state <- H.lift $ loadGame handle
    H.put state
    pure unit

component :: forall query output left.
    String -> H.Component HH.HTML query String output (Async left)
component handle = H.mkComponent
    { initialState: const Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just $ Init handle
        , receive = Just <<< Init
        }
    }

game :: forall query children left.
    String -> HH.ComponentHTML query (game :: Slot Unit | children) (Async left)
game handle =
    HH.slot (SProxy :: SProxy "game") unit (component handle) handle absurd

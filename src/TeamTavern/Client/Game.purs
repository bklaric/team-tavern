module TeamTavern.Client.Game (Query, Slot, game) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
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

data Query send = Init String send

data State
    = Empty
    | Game View.OkContent Boolean Boolean
    | NotFound
    | Error

type Slot = H.Slot Query Void

type ChildSlots =
    ( profiles :: Anchor.Slot Int
    , createProfile :: Anchor.Slot Unit
    , edit :: Anchor.Slot Unit
    )

render :: forall left. State -> H.ComponentHTML Query ChildSlots (Async left)
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

eval :: forall left.
    Query ~> H.HalogenM State Query ChildSlots Void (Async left)
eval (Init handle send) = do
    state <- H.lift $ loadGame handle
    H.put state
    pure send

component :: forall input left.
    String -> H.Component HH.HTML Query input Void (Async left)
component handle =
    H.component
        { initialState: const Empty
        , render
        , eval
        , receiver: const Nothing
        , initializer: Just $ Init handle unit
        , finalizer: Nothing
        }

game :: forall query children left.
    String -> HH.ComponentHTML query (game :: Slot Unit | children) (Async left)
game handle =
    HH.slot (SProxy :: SProxy "game") unit (component handle) unit absurd

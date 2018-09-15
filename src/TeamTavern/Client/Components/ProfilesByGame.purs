module TeamTavern.Client.Components.ProfilesByGame
    (Query, Slot, profilesByGame) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Profile.ViewByGame.Response as ViewByGame

data Query send = Init String send

data State
    = Empty
    | Profiles ViewByGame.OkContent
    | Error

type Slot = H.Slot Query Void

type ChildSlots = ( players :: Anchor.Slot Int )

render :: forall left. State -> H.ComponentHTML Query ChildSlots (Async left)
render Empty = HH.div_ []
render (Profiles profiles) = HH.ul_ $
    profiles # mapWithIndex \index { nickname, summary } -> HH.li_
        [ HH.h3_ [ navigationAnchorIndexed (SProxy :: SProxy "players") index
            { path: "/players/" <> nickname, text: nickname } ]
        , HH.p_ [ HH.text summary ]
        ]
render Error = HH.p_ [ HH.text
    "There has been an error loading game profiles. Please try again later." ]

loadProfiles :: forall left. String -> Async left State
loadProfiles handle = Async.unify do
    response <-  Fetch.fetch_ ("/api/games/" <> handle <> "/profiles")
        # lmap (const Error)
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Error)
        _ -> Async.left Error
    pure $ Profiles content

eval :: forall left.
    Query ~> H.HalogenM State Query ChildSlots Void (Async left)
eval (Init handle send) = do
    state <- H.lift $ loadProfiles handle
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

profilesByGame
    :: forall query children left
    .  String
    -> HH.ComponentHTML query (profiles :: Slot Unit | children) (Async left)
profilesByGame handle =
    HH.slot (SProxy :: SProxy "profiles") unit (component handle) unit absurd

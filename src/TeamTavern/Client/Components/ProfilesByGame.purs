module TeamTavern.Client.Components.ProfilesByGame
    (Query, State', State, Slot, profilesByGame) where

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
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Profile.ViewByGame.Response as ViewByGame

data Query send = Init send

data State'
    = Empty
    | Profiles ViewByGame.OkContent
    | Error

type State =
    { handle :: String
    , state :: State'
    }

type Slot = H.Slot Query Void

type ChildSlots = ( players :: Anchor.Slot Int )

render :: forall left. State -> H.ComponentHTML Query ChildSlots (Async left)
render { state: Empty } = HH.div_ []
render { state: Profiles profiles } = HH.ul_ $
    profiles # mapWithIndex \index { nickname, summary } -> HH.li_
        [ HH.h3_ [ HH.slot (SProxy :: SProxy "players") index navigationAnchor
            { path: "/players/" <> nickname, text: nickname } absurd ]
        , HH.p_ [ HH.text summary ]
        ]
render { state: Error } = HH.h2_
    [ HH.text "There has been an error loading game profiles." ]

loadProfiles :: forall left. String -> Async left State'
loadProfiles handle = Async.unify do
    response <-  Fetch.fetch_ ("/api/games/" <> handle <> "/profiles")
        # lmap (const Error)
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Error)
        _ -> Async.left Error
    pure $ Profiles content

eval :: forall left.
    Query ~> H.HalogenM State Query ChildSlots Void (Async left)
eval (Init send) = do
    { handle } <- H.get
    state <- H.lift $ loadProfiles handle
    H.put { handle, state }
    pure send

profilesByGame :: forall left.
    H.Component HH.HTML Query String Void (Async left)
profilesByGame =
    H.component
        { initialState: { handle: _, state: Empty }
        , render
        , eval
        , receiver: const Nothing
        , initializer: Just $ Init unit
        , finalizer: Nothing
        }

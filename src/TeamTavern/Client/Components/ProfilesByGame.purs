module TeamTavern.Client.Components.ProfilesByGame (Slot, profilesByGame) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen (ClassName(..), defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Server.Profile.ViewByGame.SendResponse as ViewByGame

data Action = Init String

data State
    = Empty
    | Profiles ViewByGame.OkContent

type Slot = H.Slot (Const Void) Void

type ChildSlots = (players :: Anchor.Slot Int)

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render Empty = HH.div_ []
render (Profiles profiles) = HH.div_ $
    [ HH.h3 [ HP.class_ $ ClassName "card-header"] [ HH.text "Profiles" ] ] <>
    (profiles # mapWithIndex \index { nickname, summary } ->
        HH.div [ HP.class_ $ ClassName "card" ] $
        [ HH.h3_ [ navigationAnchorIndexed (SProxy :: SProxy "players") index
            { path: "/players/" <> nickname, content: HH.text nickname } ]
        ] <> (summary <#> \paragraph -> HH.p_ [ HH.text paragraph ])
    )

loadProfiles :: forall left. String -> Async left State
loadProfiles handle = Async.unify do
    response <-  Fetch.fetch_ ("/api/profiles?handle=" <> handle)
        # lmap (const Empty)
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Empty)
        _ -> Async.left Empty
    pure $ Profiles content

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (Init handle) = do
    state <- H.lift $ loadProfiles handle
    H.put state
    pure unit

component :: forall output left query.
    String -> H.Component HH.HTML query String output (Async left)
component handle = mkComponent
    { initialState: const Empty
    , render
    , eval: mkEval $ defaultEval
        { handleAction = handleAction
        , initialize = Just $ Init handle
        , receive = Just <<< Init
        }
    }

profilesByGame
    :: forall query children left
    .  String
    -> HH.ComponentHTML
        query (profilesByGame :: Slot Unit | children) (Async left)
profilesByGame handle = HH.slot
    (SProxy :: SProxy "profilesByGame") unit (component handle) handle absurd

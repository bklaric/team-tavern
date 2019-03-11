module TeamTavern.Client.Components.ProfilesByPlayer
    (Slot, profilesByPlayer) where

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
import TeamTavern.Profile.ViewByPlayer.SendResponse as ViewByPlayer

data Action = Init String

data State
    = Empty
    | Profiles ViewByPlayer.OkContent

type Slot = H.Slot (Const Void) Void

type ChildSlots = (games :: Anchor.Slot Int)

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render Empty = HH.div_ []
render (Profiles profiles) = HH.div_ $
    [ HH.h3_ [ HH.text "Profiles" ] ] <>
    (profiles # mapWithIndex \index { handle, title, summary } ->
        HH.div [ HP.class_ $ ClassName "profile-item" ]
        [ HH.h3_ [ navigationAnchorIndexed (SProxy :: SProxy "games") index
            { path: "/game/" <> handle, text: title } ]
        , HH.p_ [ HH.text summary ]
        ])

loadProfiles :: forall left. String -> Async left State
loadProfiles nickname = Async.unify do
    response <-  Fetch.fetch_ ("/api/profiles?nickname=" <> nickname)
        # lmap (const Empty)
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Empty)
        _ -> Async.left Empty
    pure $ Profiles content

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (Init nickname) = do
    state <- H.lift $ loadProfiles nickname
    H.put state
    pure unit

component :: forall output left initState query.
    String -> H.Component HH.HTML query initState output (Async left)
component nickname = mkComponent
    { initialState: const Empty
    , render
    , eval: mkEval $ defaultEval
        { handleAction = handleAction
        , initialize = Just $ Init nickname
        }
    }

profilesByPlayer
    :: forall query children left
    .  String
    -> HH.ComponentHTML query (profiles :: Slot Unit | children) (Async left)
profilesByPlayer nickname =
    HH.slot (SProxy :: SProxy "profiles") unit (component nickname) unit absurd

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
import Halogen as H
import Halogen.HTML as HH
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Player.View.Response as View

data Query send = Init String send

data State
    = Empty
    | Player View.OkContent
    | NotFound
    | Error

type Slot = H.Slot Query Void

type ChildSlots = ( games :: Anchor.Slot Int )

render :: forall left. State -> H.ComponentHTML Query ChildSlots (Async left)
render Empty = HH.div_ []
render (Player { nickname, about, profiles }) = HH.div_
    [ HH.h2_ [ HH.text nickname ]
    , HH.p_ [ HH.text about ]
    , HH.ul_ $
        profiles # mapWithIndex \index { handle, name, summary } -> HH.li_
            [ HH.h3_ [ navigationAnchorIndexed (SProxy :: SProxy "games") index
                { path: "/games/" <> handle, text: name } ]
            , HH.p_ [ HH.text summary ]
            ]
    ]
render NotFound = HH.p_ [ HH.text "Player could not be found." ]
render Error = HH.p_ [ HH.text
    "There has been an error loading the player. Please try again later." ]

loadPlayer :: forall left. String -> Async left State
loadPlayer nickname = Async.unify do
    response <- Fetch.fetch_ ("/api/players/" <> nickname) # lmap (const Error)
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Error)
        404 -> Async.left NotFound
        _ -> Async.left Error
    pure $ Player content

eval :: forall left.
    Query ~> H.HalogenM State Query ChildSlots Void (Async left)
eval (Init nickname send) = do
    state <- H.lift $ loadPlayer nickname
    H.put state
    pure send

component :: forall left.
    String -> H.Component HH.HTML Query String Void (Async left)
component nickname = H.component
    { initialState: const Empty
    , render
    , eval
    , receiver: \nickname' -> Just $ Init nickname' unit
    , initializer: Just $ Init nickname unit
    , finalizer: Nothing
    }

player
    :: forall query children left
    .  String
    -> HH.ComponentHTML query (player :: Slot Unit | children) (Async left)
player nickname = HH.slot
    (SProxy :: SProxy "player") unit (component nickname) nickname absurd

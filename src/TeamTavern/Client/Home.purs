module TeamTavern.Client.Home (Query, State, Slot, ChildSlots, home) where

import Prelude
import Prim.Row

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Symbol (class IsSymbol, SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Game.ViewAll.Response (OkContent)

data Query send = Init send

data State
    = Games OkContent
    | Error

type Slot = H.Slot Query Void

type ChildSlots = (games :: Anchor.Slot Int)

render :: forall left. State -> H.ComponentHTML Query ChildSlots (Async left)
render (Games games) = HH.ul [] $
    games # mapWithIndex \index { name, handle, description } -> HH.li []
        [ HH.h2_ [ HH.slot (SProxy :: SProxy "games") index navigationAnchor { path: "/games/" <> handle, text: name } absurd ]
        , HH.p_ [ HH.text description ]
        ]
render Error = HH.h2_ [ HH.text "There has been an error loading the games." ]

eval :: forall left. Query ~> H.HalogenM State Query ChildSlots Void (Async left)
eval (Init send) = do
    response' <- H.lift $ Async.attempt $ Fetch.fetch
        "/api/games"
        (Fetch.method := GET)
    newState <- case response' of
        Left error -> pure Error
        Right response -> H.lift
            case FetchRes.status response of
            200 -> FetchRes.text response <#> Json.readJSON >>=
                case _ of
                Left errors -> pure Error
                Right (content :: OkContent) -> pure $ Games content
            _ -> pure Error
    H.put newState
    pure send

component :: forall left. H.Component HH.HTML Query State Void (Async left)
component =
    H.component
        { initialState: identity
        , render
        , eval
        , receiver: const Nothing
        , initializer: Just $ Init unit
        , finalizer: Nothing
        }

home label = HH.slot label unit component (Games []) absurd

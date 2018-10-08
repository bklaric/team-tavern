module TeamTavern.Client.Home (Query, Slot, home) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Symbol (SProxy(..))
import Effect.Class.Console (log)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Game.ViewAll.Response (OkContent)

data Query send = Init send | Receive send

data State
    = Games OkContent
    | Error

type Slot = H.Slot Query Void

type ChildSlots = (games :: Anchor.Slot Int)

render :: forall left. State -> H.ComponentHTML Query ChildSlots (Async left)
render (Games games) = HH.div_ $
    games # mapWithIndex \index { title, handle, description } ->
        HH.div [ HP.class_ $ ClassName "game-item" ]
        [ HH.h2_ [ navigationAnchorIndexed (SProxy :: SProxy "games") index
            { path: "/games/" <> handle, text: title } ]
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
eval (Receive send) = log "Lol home" *> pure send

component :: forall left. H.Component HH.HTML Query State Void (Async left)
component =
    H.component
        { initialState: identity
        , render
        , eval
        , receiver: const $ Just $ Receive unit
        , initializer: Just $ Init unit
        , finalizer: Nothing
        }

home :: forall query children left.
    HH.ComponentHTML query (home :: Slot Unit | children) (Async left)
home = HH.slot (SProxy :: SProxy "home") unit component (Games []) absurd

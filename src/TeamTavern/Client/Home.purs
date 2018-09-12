module TeamTavern.Client.Home where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import TeamTavern.Game.ViewAll.LoadGames (GameViewModel)
import TeamTavern.Game.ViewAll.Response (OkContent)

data Query send = Init send

data State
    = Games (OkContent)
    | Error

type Slot = H.Slot Query Void

render :: forall left. State -> H.ComponentHTML Query () (Async left)
render (Games games) = HH.ul [] $
    games # map \{ name, handle, description } -> HH.li []
        [ HH.h2_ [ HH.a [ HP.href $ "/api/games/" <> handle ] [ HH.text name ] ]
        , HH.p_ [ HH.text description ]
        ]
render Error = HH.h2_ [ HH.text "There has been an error loading the games." ]

eval :: forall left. Query ~> H.HalogenM State Query () Void (Async left)
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
        { initialState: const $ Games []
        , render
        , eval
        , receiver: const Nothing
        , initializer: Just $ Init unit
        , finalizer: Nothing
        }

home label = HH.slot label unit component (Games []) absurd

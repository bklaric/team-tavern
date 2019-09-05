module TeamTavern.Client.Game (Slot, game) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Client.Game.FilterProfiles (filterProfiles)
import TeamTavern.Client.Game.FilterProfiles as FilterProfiles
import TeamTavern.Client.Game.Profiles (gameProfiles)
import TeamTavern.Client.Game.Profiles as Profiles
import TeamTavern.Client.Script.Cookie (getPlayerId)
import TeamTavern.Server.Game.View.SendResponse as View

data Action
    = Init String
    | ApplyFilters (Array FilterProfiles.Field)

data State
    = Empty
    | Game View.OkContent
    | NotFound
    | Error

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( gameProfiles :: Profiles.Slot Unit
    , filterProfiles :: FilterProfiles.Slot Unit
    )

filterableFields
    :: Array
        { key :: String
        , label :: String
        , options :: Maybe (Array
            { key :: String
            , option :: String
            })
        , type :: Int
        }
    -> Array FilterProfiles.Field
filterableFields fields = fields # Array.mapMaybe case _ of
    { key, label, type: type', options: Just options }
    | type' == 2 || type' == 3 -> Just { key, label, options }
    _ -> Nothing

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render Empty = HH.div_ []
render (Game game') = HH.div_
    [ HH.h2 [ HP.class_ $ HH.ClassName "content-title" ]
        [ HH.text game'.title ]
    , filterProfiles (filterableFields game'.fields)
        (\(FilterProfiles.ApplyFilters filters) -> Just $ ApplyFilters filters)
    , gameProfiles game'
    ]
render NotFound = HH.p_ [ HH.text "Game could not be found." ]
render Error = HH.p_ [ HH.text
    "There has been an error loading the game. Please try again later." ]

loadGame :: forall left. String -> Async left State
loadGame handle = Async.unify do
    response <- Fetch.fetch_ ("/api/games/by-handle/" <> handle)
        # lmap (const Error)
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Error)
        404 -> Async.left NotFound
        _ -> Async.left Error
    playerId <- Async.fromEffect getPlayerId
    pure $ Game content

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (Init handle) = do
    state <- H.lift $ loadGame handle
    H.put state
    pure unit
handleAction (ApplyFilters filters) =
    void $ H.query (SProxy :: SProxy "gameProfiles") unit
        (Profiles.ApplyFilters filters unit)

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

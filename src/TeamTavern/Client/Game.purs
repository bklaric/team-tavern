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
import Simple.JSON.Async as Json
import TeamTavern.Client.Profile.ProfileFilters (filterProfiles)
import TeamTavern.Client.Profile.ProfileFilters as FilterProfiles
import TeamTavern.Client.Game.GameHeader as GameHeader
import TeamTavern.Client.Game.Profiles (gameProfiles)
import TeamTavern.Client.Game.Profiles as Profiles
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Server.Game.View.SendResponse as View

data Input = Input GameHeader.Handle GameHeader.Tab

data Action
    = Init
    | Receive Input
    | ApplyFilters (Array FilterProfiles.Field)

data State
    = Empty Input
    | Game View.OkContent GameHeader.Tab
    | NotFound
    | Error

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( gameHeader :: H.Slot (Const Void) Void Unit
    , gameProfiles :: Profiles.Slot Unit
    , filterProfiles :: FilterProfiles.Slot Unit
    )

filterableFields
    :: Array
        { key :: String
        , label :: String
        , icon :: String
        , required :: Boolean
        , domain :: Maybe String
        , options :: Maybe (Array
            { key :: String
            , option :: String
            })
        , type :: Int
        }
    -> Array FilterProfiles.Field
filterableFields fields = fields # Array.mapMaybe case _ of
    { key, label, icon, type: type', domain, options: Just options }
    | type' == 2 || type' == 3 -> Just { key, label, icon, domain, options }
    _ -> Nothing

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Game game' tab) = let
    gameHeader =
        HH.slot (SProxy :: SProxy "gameHeader") unit GameHeader.component
        (GameHeader.Input game'.handle game'.title tab) absurd
    in
    HH.div_
    [ gameHeader
    , filterProfiles (filterableFields game'.fields)
        (\(FilterProfiles.ApplyFilters filters) -> Just $ ApplyFilters filters)
    , gameProfiles game'
    ]
render NotFound = HH.p_ [ HH.text "Game could not be found." ]
render Error = HH.p_ [ HH.text
    "There has been an error loading the game. Please try again later." ]

loadGame :: forall left. String -> GameHeader.Tab -> Async left State
loadGame handle tab = Async.unify do
    response <- Fetch.fetch_ ("/api/games/by-handle/" <> handle)
        # lmap (const Error)
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Error)
        404 -> Async.left NotFound
        _ -> Async.left Error
    pure $ Game content tab

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Init = do
    state <- H.get
    case state of
        Empty (Input handle tab) -> do
            state <- H.lift $ loadGame handle tab
            H.put state
            let handleOrTitle =
                    case state of
                    Game { title } _ -> title
                    _ -> handle
            H.lift $ Async.fromEffect do
                setMetaTitle $ "Find " <> handleOrTitle <> " players | TeamTavern"
                setMetaDescription $ "Browse and filter " <> handleOrTitle <> " players on TeamTavern and find your ideal teammates."
                setMetaUrl
        _ -> pure unit
handleAction (Receive (Input handle tab)) =
    H.modify_ case _ of
        Game content _ -> Game content tab
        state -> state
handleAction (ApplyFilters filters) =
    void $ H.query (SProxy :: SProxy "gameProfiles") unit
        (Profiles.ApplyFilters filters unit)

component :: forall query output left.
    H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        , receive = Just <<< Receive
        }
    }

game :: forall query children left.
    String -> GameHeader.Tab -> HH.ComponentHTML query (game :: Slot Unit | children) (Async left)
game handle tab =
    HH.slot (SProxy :: SProxy "game") unit component (Input handle tab) absurd

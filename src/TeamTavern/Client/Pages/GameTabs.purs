module TeamTavern.Client.Pages.GameTabs where

import Prelude

import Async (Async)
import Async as Async
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (onMatch)
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Content (actualContent)
import TeamTavern.Client.Pages.Profiles (profiles)
import TeamTavern.Client.Pages.Profiles.GameHeader (ProfileTab(..), Tab(..), gameHeader)
import TeamTavern.Client.Script.RenderReady (appendRenderReadyNotFound, appendRenderReadyUnavailable)
import TeamTavern.Client.Shared.Fetch (fetchPath)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Routes.Game.ViewGame (ViewGame)
import TeamTavern.Routes.Game.ViewGame as ViewGame
import Type.Proxy (Proxy(..))

type Input = { handle :: String, tab :: Tab }

data State
    = Empty Input
    | Loaded { game :: ViewGame.OkContent, tab :: Tab }
    | NotFound
    | Error

data Action = Initialize | Receive Input

type ChildSlots =
    ( profiles :: Slot___
    )

render :: ∀ left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Loaded { game, tab }) = actualContent $
    gameHeader { title: game.title, description: game.description, tab }
    <>
    [ case tab of
        Profiles Players -> profiles { game, tab: Players }
        Profiles Teams -> profiles { game, tab: Teams }
    ]
render NotFound = HH.p_ [ HH.text "Game could not be found." ]
render Error = HH.p_ [ HH.text "There has been an error loading the game. Please try again later." ]

handleAction :: ∀ output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    case state of
        Empty input -> handleAction $ Receive input
        _ -> pure unit
handleAction (Receive input) = do
    state <- H.get
    case state of
        Loaded loaded | loaded.game.handle == input.handle ->
            H.put $ Loaded loaded { tab = input.tab }
        _ -> do
            result <- H.lift $ Async.attempt $
                fetchPath (Proxy :: _ ViewGame) { handle: input.handle }
            case result of
                Left _ -> appendRenderReadyUnavailable *> H.put Error
                Right response -> response # onMatch
                    { ok: \game -> H.put $ Loaded { game, tab: input.tab }
                    , notFound: const do
                        appendRenderReadyNotFound
                        H.put NotFound
                    }
                    (const $ appendRenderReadyUnavailable *> H.put Error)

component :: ∀ query output left. H.Component query Input output (Async left)
component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

gameTabs :: ∀ query children left.
    Input -> HH.ComponentHTML query (gameTabs :: Slot___ | children) (Async left)
gameTabs input = HH.slot (Proxy :: _ "gameTabs") unit component input absurd

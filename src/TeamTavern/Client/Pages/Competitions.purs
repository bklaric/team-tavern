module TeamTavern.Client.Pages.Competitions where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Script.Request (get)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.ViewCompetitions as ViewCompetitions
import TeamTavern.Routes.ViewGame as ViewGame

type Input = { game :: ViewGame.OkContent }

data State
    = Empty Input
    | Loaded { game :: ViewGame.OkContent, competitions :: ViewCompetitions.OkContent }
    | Error

data Action = Initialize | Receive Input

type Slot = H.Slot (Const Void) Void Unit

render :: forall slots action. State -> HH.HTML slots action
render (Empty _) = HH.div_ []
render (Loaded { game, competitions: competitions' }) = HH.div_ $
    mapFlipped competitions' \competition ->
        HH.div [ HS.class_ "competition-card" ] [ HH.text competition.name ]
render Error =
    HH.p_ [ HH.text "There has been an error loading competitions. Please try again later." ]

handleAction :: forall slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    case state of
        Empty input -> handleAction $ Receive input
        _ -> pure unit
handleAction (Receive { game }) = do
    competitions' <- H.lift $ get $ "/api/games/" <> game.handle <> "/competitions"
    H.put case competitions' of
        Nothing -> Error
        Just competitions'' -> Loaded { game, competitions: competitions'' }

component :: forall query output left. H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

competitions :: forall query children left.
    Input -> HH.ComponentHTML query (competitions :: Slot | children) (Async left)
competitions input = HH.slot (SProxy :: SProxy "competitions") unit component input absurd

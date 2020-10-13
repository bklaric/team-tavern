module TeamTavern.Client.Pages.Team (Input, Slot, team) where

import Prelude

import Async (Async)
import Async as Async
import CSS as CSS
import Client.Components.Copyable as Copyable
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Button (regularIconButton)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Pages.Team.Details (details)
import TeamTavern.Client.Pages.Team.EditTeam (editTeam)
import TeamTavern.Client.Pages.Team.EditTeam as EditTeam
import TeamTavern.Client.Pages.Team.Profiles (profiles)
import TeamTavern.Client.Pages.Team.Status (Status(..), getStatus)
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Request (get)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Server.Team.View (Team)

type Input = { handle :: String }

data State
    = Empty Input
    | Loaded { team :: Team, status :: Status, showEditModal :: Boolean }
    | NotFound
    | Error

data Action = Initialize | ShowEditModal | HideEditModal

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots =
    ( messageOwner :: NavigationAnchor.Slot Unit
    , discordServer :: Copyable.Slot
    , games :: Anchor.Slot String
    , editTeam :: EditTeam.Slot
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Loaded { team: team', status, showEditModal } ) =
    HH.div_  $
    [ HH.div [ HS.class_ "content-title" ]
        [ HH.div [ HS.class_ "content-title-left" ]
            [ HH.h1 [ HS.class_ "content-title-text" ]
                [ HH.text team'.name ]
            ]
        , HH.div [ HS.class_ "content-title-right" ]
            case status of
            SignedInOwner ->
                [ regularIconButton "fas fa-edit" "Edit team" ShowEditModal
                ]
            _ ->
                [ navigationAnchor (SProxy :: SProxy "messageOwner")
                    { path: "/conversations/" <> team'.owner
                    , content: HH.span [ HC.style $ CSS.fontWeight $ CSS.weight 500.0 ]
                        [ HH.i [ HP.class_ $ H.ClassName "fas fa-envelope button-icon" ] []
                        , HH.text "Message team owner"
                        ]
                    }
                ]
        ]
    , HH.p [ HS.class_ "content-description" ]
        [ HH.text "This is a team, lmao!" ]
    , details team'
    , profiles team'.profiles
    ]
    <>
    if showEditModal
    then [ editTeam team' (const $ Just HideEditModal) ]
    else [ ]
render NotFound = HH.p_ [ HH.text "Team could not be found." ]
render Error = HH.p_ [ HH.text "There has been an error loading the team. Please try again later." ]

loadTeam :: forall left. String -> Async left (Maybe Team)
loadTeam handle = do
    timezone <- getClientTimezone
    get $ "/api/teams/by-handle/" <> handle <> "?timezone=" <> timezone

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    case state of
        Empty { handle } -> do
            team' <- H.lift $ loadTeam handle
            case team' of
                Just team'' -> do
                    status <- getStatus team''.owner
                    H.put $ Loaded { team: team'', status, showEditModal: false }
                _ -> pure unit
        _ -> pure unit
    H.lift $ Async.fromEffect do
        setMetaTitle $ "aoeu" <> " | TeamTavern"
        setMetaDescription $ "View profiles by player " <> "aoeueuue" <> " on TeamTavern."
        setMetaUrl
handleAction ShowEditModal =
    H.modify_
    case _ of
    Loaded state -> Loaded state { showEditModal = true }
    state -> state
handleAction HideEditModal =
    H.modify_
    case _ of
    Loaded state -> Loaded state { showEditModal = false }
    state -> state

component :: forall query output left.
    H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

team :: forall query children left.
    Input -> HH.ComponentHTML query (team :: Slot | children) (Async left)
team handle = HH.slot (SProxy :: SProxy "team") unit component handle absurd

module TeamTavern.Client.Pages.Player.Teams (Input, Slot, teams) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Pages.Player.CreateTeam (createTeam)
import TeamTavern.Client.Pages.Player.Types (PlayerStatus(..))
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Client.Script.Request (get)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Server.Team.ViewByOwner (Team)

type Input =
    { nickname :: String
    , status :: PlayerStatus
    }

data State
    = Empty Input
    | Loaded
        { nickname :: String
        , status :: PlayerStatus
        , modalShown :: Boolean
        , teams :: Array Team
        }

data Action
    = Initialize
    | ShowModal
    | HandleModalOutput (Modal.Output Void)

type Slot = H.Slot (Const Void) Void Unit

renderTeams teams' status =
    if Array.null teams'
    then Array.singleton $
        HH.div [ HS.class_ "card-section" ]
        [ HH.p_
            [ HH.text
                case status of
                SamePlayer -> "You haven't created any teams."
                _ -> "This player hasn't created any teams."
            ]
        ]
    else
        teams' <#> \team ->
            HH.div [ HS.class_ "card-section" ]
            [ HH.h3 [ HS.class_ "team-heading" ]
                [ navigationAnchorIndexed (SProxy :: SProxy "team") team.handle
                    { path: "/teams/" <> team.handle
                    , content: HH.text team.name
                    }
                , divider
                , HH.span [ HP.class_ $ HH.ClassName "profile-updated" ]
                    [ HH.text $ lastUpdated team.updatedSeconds ]
                ]
            ]

render (Empty _) = HH.div_ []
render (Loaded state) =
    HH.div [ HS.class_ "card" ] $
    [ HH.h2 [ HS.class_ "card-title" ] $
        [ HH.text "Teams" ]
        <>
        [ HH.button
            [ HP.class_ $ HH.ClassName "primary-button"
            , HE.onClick $ const $ Just ShowModal
            ]
            [ HH.i [ HP.class_ $ HH.ClassName "fas fa-user-plus button-icon" ] []
            , HH.text "Create team"
            ]
        ]
    ]
    <> renderTeams state.teams state.status
    <>
    if state.modalShown
    then [ createTeam (Just <<< HandleModalOutput) ]
    else []

loadTeams :: forall left. String -> Async left (Maybe (Array Team))
loadTeams nickname = get $ "/api/teams/by-owner/" <> nickname

handleAction :: forall action slots output left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    case state of
        Empty { nickname } -> do
            teams' <- H.lift $ loadTeams nickname
            case teams' of
                Just teams'' ->
                    H.put $ Loaded
                        { nickname: ""
                        , status: SignedOut
                        , modalShown: false
                        , teams: teams''
                        }
                _ -> pure unit
        _ -> pure unit
handleAction ShowModal =
    H.modify_ case _ of
        Loaded state -> Loaded state { modalShown = true }
        state -> state
handleAction (HandleModalOutput _) =
    H.modify_ case _ of
        Loaded state -> Loaded state { modalShown = false }
        state -> state

component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

teams input = HH.slot (SProxy :: SProxy "teams") unit component input absurd

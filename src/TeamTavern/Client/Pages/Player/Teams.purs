module TeamTavern.Client.Pages.Player.Teams where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (intercalate)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Halogen (defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.ModalDeclarative as Modal
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Pages.Player.CreateTeam (createTeam)
import TeamTavern.Client.Pages.Player.CreateTeam as CreateTeam
import TeamTavern.Client.Pages.Player.EditTeamProfile (editTeamProfile)
import TeamTavern.Client.Pages.Player.EditTeamProfile as EditProfile
import TeamTavern.Client.Pages.Player.Types (Nickname, PlayerStatus(..))
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Server.Profile.ViewTeamProfilesByPlayer.SendResponse as ViewTeamProfilesByPlayer

type Team = { handle :: String, name :: String }

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
    | HandleModalOutput (Modal.Output CreateTeam.Output)

type Slot = H.Slot (Const Void) Void Unit

renderTeams (teams' :: Array { handle :: String, name :: String }) =
    teams' <#> \team ->
        HH.div [ HS.class_ "card-section" ]
        [ HH.h3 [ HS.class_ "team-heading" ]
            [ navigationAnchorIndexed (SProxy :: SProxy "team") team.handle
                { path: "/teams/" <> team.handle
                , content: HH.text team.name
                }
            , divider
            , HH.span [ HP.class_ $ HH.ClassName "profile-updated" ]
                [ HH.text $ "Updated 12 days ago" ]
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
    <> renderTeams state.teams
    <>
    if state.modalShown
    then [ createTeam { nickname: state.nickname } (Just <<< HandleModalOutput) ]
    else []

handleAction Initialize =
    H.put $ Loaded
        { nickname: ""
        , status: SignedOut
        , modalShown: false
        , teams:
            [ { handle: "niggaz", name: "Niggaz from da hood" }
            , { handle: "cunts", name: "Aussie cunts" }
            ]
        }
handleAction ShowModal =
    H.modify_ case _ of
        Loaded state -> Loaded state { modalShown = true }
        state -> state
handleAction (HandleModalOutput (Modal.OutputRaised (CreateTeam.TeamCreated { handle }))) =
    H.liftEffect $ navigate_ $ "/teams/" <> handle
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

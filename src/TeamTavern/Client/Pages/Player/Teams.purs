module TeamTavern.Client.Pages.Player.Teams where

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
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Pages.Player.CreateTeam (createTeam)
import TeamTavern.Client.Pages.Player.CreateTeam as CreateTeam
import TeamTavern.Client.Pages.Player.Types (PlayerStatus(..))
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Snippets.Class as HS

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
    <> renderTeams state.teams state.status
    <>
    if state.modalShown
    then [ createTeam { nickname: state.nickname } (Just <<< HandleModalOutput) ]
    else []

get url = Async.unify do
    response <- Fetch.fetch_ url # lmap (const Nothing)
    case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Nothing)
        _ -> Async.left Nothing

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
handleAction (HandleModalOutput (Modal.OutputRaised { handle })) =
    navigate_ $ "/teams/" <> handle
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

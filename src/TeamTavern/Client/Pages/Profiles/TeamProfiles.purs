module TeamTavern.Client.Pages.Profiles.TeamProfiles (TeamProfile, Input, Output(..), Slot, teamProfiles) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Card (card, cardHeader, cardHeading, cardSection, cardSubheading)
import TeamTavern.Client.Components.Detail (detailColumn, detailColumnHeading, detailColumns, textDetail)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Components.Pagination (pagination)
import TeamTavern.Client.Components.Profile (profileHeader, profileHeaderItem, profileHeading, profileSubheading)
import TeamTavern.Client.Components.Team.ProfileDetails (profileDetails')
import TeamTavern.Client.Components.Team.TeamDetails (teamDetails)
import TeamTavern.Client.Script.Cookie (PlayerInfo)
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Server.Profile.ViewTeamProfilesByGame.LoadProfiles (pageSize)

type TeamProfile =
    { owner :: String
    , handle :: String
    , name :: String
    , website :: Maybe String
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , discordServer :: Maybe String
    , weekdayOnline :: Maybe
        { from :: String
        , to :: String
        }
    , weekendOnline :: Maybe
        { from :: String
        , to :: String
        }
    , about :: Array String
    , fieldValues :: Array
        { field ::
            { ilk :: Int
            , key :: String
            , label :: String
            , icon :: String
            }
        , options :: Array
            { key :: String
            , label :: String
            }
        }
    , newOrReturning :: Boolean
    , ambitions :: Array String
    , updated :: String
    , updatedSeconds :: Number
    }

type Input =
    { profiles :: Array TeamProfile
    , profileCount :: Int
    , playerInfo :: Maybe PlayerInfo
    , page :: Int
    }

type State = Input

data Action
    = Receive Input
    | ChangePage Int

data Output = PageChanged Int

type Slot = H.Slot (Const Void) Output Unit

type ChildSlots =
    ( teams :: Anchor.Slot String
    , messageOwner :: Anchor.Slot String
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { profiles, profileCount, playerInfo, page } =
    card $
    [ cardHeader $
        [ HH.div_ $
            [ cardHeading "Team profiles"
            , divider
            , cardSubheading $
                ( if profileCount == 0
                    then "Showing 0"
                    else
                        "Showing " <> show (1 + ((page - 1) * pageSize))
                        <> " - " <> show (min profileCount (page * pageSize))
                        <> " out of " <> show profileCount
                )
                <> " teams"
            ]
        ]
    ]
    <>
    if Array.null profiles
    then [ cardSection [ HH.p_ [ HH.text "No profiles satisfy specified filters." ] ] ]
    else
    ( profiles <#> \profile -> let
        teamDetails' = teamDetails profile
        profileDetails'' = profileDetails' profile.fieldValues profile.newOrReturning
        about = textDetail profile.about
        ambitions = textDetail profile.ambitions
        in
        cardSection $
        [ profileHeader $
            [ profileHeaderItem
                [ profileHeading (SProxy :: SProxy "teams") profile.handle
                    ("/teams/" <> profile.handle) profile.name
                , divider
                , profileSubheading $ "Updated " <> lastUpdated profile.updatedSeconds
                ]
            ]
            <>
            case playerInfo of
            Just { nickname } | nickname /= profile.owner ->
                [ profileHeaderItem
                    [ navigationAnchorIndexed (SProxy :: SProxy "messageOwner") profile.owner
                        { path: "/conversations/" <> profile.owner
                        , content: HH.span_
                            [ HH.i [ HP.class_ $ H.ClassName "fas fa-envelope button-icon" ] []
                            , HH.text "Message team owner"
                            ]
                        }
                    ]
                ]
            _ -> []
        ]
        <>
        if (not $ Array.null teamDetails') || (not $ Array.null profileDetails'')
            || (not $ Array.null about) || (not $ Array.null ambitions)
        then
            [ detailColumns $
                ( if (not $ Array.null teamDetails') || (not $ Array.null profileDetails'')
                    then
                        [ detailColumn $
                            ( if not $ Array.null teamDetails'
                                then [ detailColumnHeading "Team details" ] <> teamDetails'
                                else []
                            )
                            <>
                            ( if not $ Array.null profileDetails''
                                then [ detailColumnHeading "Profile details" ] <> profileDetails''
                                else []
                            )
                        ]
                    else []
                )
                <>
                ( if (not $ Array.null about) || (not $ Array.null ambitions)
                    then
                        [ detailColumn $
                            ( if not $ Array.null about
                                then [ detailColumnHeading "About" ] <> about
                                else []
                            )
                            <>
                            ( if not $ Array.null ambitions
                                then [ detailColumnHeading "Ambitions" ] <> ambitions
                                else []
                            )
                        ]
                    else []
                )
            ]
        else []
    )
    <> [ pagination page profileCount ChangePage ]

handleAction :: forall left. Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive input) = H.put input
handleAction (ChangePage page) = H.raise $ PageChanged page

component :: forall query left. H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

teamProfiles
    :: forall children action left
    .  Input
    -> (Output -> Maybe action)
    -> HH.ComponentHTML action (teamProfiles :: Slot | children) (Async left)
teamProfiles input handleOutput =
    HH.slot (SProxy :: SProxy "teamProfiles") unit component input handleOutput

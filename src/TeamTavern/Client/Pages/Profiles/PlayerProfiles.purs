module TeamTavern.Client.Pages.Profiles.PlayerProfiles (Fields, PlayerProfile, Input, Output(..), Slot, playerProfiles) where

import Prelude

import Async (Async)
import Client.Components.Copyable as Copyable
import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..), isNothing)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Card (cardHeader, cardHeading, cardSection, cardSubheading)
import TeamTavern.Client.Components.Detail (detailColumn, detailColumnHeading4, detailColumns, textDetail)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Components.Pagination (pagination)
import TeamTavern.Client.Components.Player.PlayerDetails (playerDetails)
import TeamTavern.Client.Components.Player.ProfileDetails (PlatformIdSlots, profileDetails')
import TeamTavern.Client.Components.Profile (profileHeader, profileHeaderItem, profileHeading, profileSubheading)
import TeamTavern.Client.Script.Cookie (PlayerInfo)
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Client.Snippets.PreventMouseDefault (preventMouseDefault)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame.LoadProfiles (pageSize)
import Web.UIEvent.MouseEvent (MouseEvent)

type Fields = Array
    { ilk :: Int
    , label :: String
    , key :: String
    , icon :: String
    , domain :: Maybe String
    , options :: Maybe (Array
        { key :: String
        , label :: String
        })
    }

type PlayerProfile =
    { nickname :: String
    , discordTag :: Maybe String
    , age :: Maybe Int
    , location :: Maybe String
    , languages :: Array String
    , microphone :: Boolean
    , weekdayOnline :: Maybe { from :: String, to :: String }
    , weekendOnline :: Maybe { from :: String, to :: String }
    , about :: Array String
    , platform :: Platform
    , platformId :: String
    , fieldValues :: Array
        { field ::
            { ilk :: Int
            , key :: String
            , label :: String
            , icon :: String
            }
        , url :: Maybe String
        , option :: Maybe
            { key :: String
            , label :: String
            }
        , options :: Maybe (Array
            { key :: String
            , label :: String
            })
        }
    , ambitions :: Array String
    , newOrReturning :: Boolean
    , updated :: String
    , updatedSeconds :: Number
    }

type Input =
    { profiles :: Array PlayerProfile
    , profileCount :: Int
    , playerInfo :: Maybe PlayerInfo
    , page :: Int
    }

type State = Input

data Action
    = Receive Input
    | ChangePage Int
    | OpenPreboarding MouseEvent

data Output = PageChanged Int | PreboardingClicked

type Slot = H.Slot (Const Void) Output Unit

type ChildSlots = PlatformIdSlots
    ( players :: Anchor.Slot String
    , messagePlayer :: Anchor.Slot String
    , discordTag :: Copyable.Slot String
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { profiles, profileCount, playerInfo, page } =
    HH.div [ HP.id_ "profiles-card", HP.class_ $ HH.ClassName "profiles-card" ] $
    [ cardHeader $
        [ HH.div_
            [ cardHeading "Player profiles"
            , divider
            , cardSubheading $
                ( if profileCount == 0
                    then "Showing 0"
                    else
                        "Showing " <> show (1 + ((page - 1) * pageSize))
                        <> " - " <> show (min profileCount (page * pageSize))
                        <> " out of " <> show profileCount
                )
                <> " players"
            ]
        ]
        <>
        if isNothing playerInfo
        then Array.singleton $
            HH.a [ HS.class_ "primary-button", HE.onClick $ Just <<< OpenPreboarding ]
            [ HH.i [ HS.class_ "fas fa-user-plus button-icon"] [], HH.text "Create player profile" ]
        else []
    ]
    <>
    if Array.null profiles
    then [ cardSection [ HH.p_ [ HH.text "No profiles satisfy specified filters." ] ] ]
    else
    ( profiles <#> \profile -> let
        playerDetails' = playerDetails profile
        profileDetails'' = profileDetails'
            profile.platform profile.platformId profile.fieldValues profile.newOrReturning
        about = textDetail profile.about
        ambitions = textDetail profile.ambitions
        in
        cardSection $
        [ profileHeader
            [ profileHeaderItem
                [ profileHeading (SProxy :: SProxy "players") profile.nickname
                    ("/players/" <> profile.nickname) profile.nickname
                , divider
                , profileSubheading $ "Updated " <> lastUpdated profile.updatedSeconds
                ]
            ]
        ]
        <>
        if (not $ Array.null playerDetails') || (not $ Array.null profileDetails'')
            || (not $ Array.null about) || (not $ Array.null ambitions)
        then
            [ detailColumns $
                ( if (not $ Array.null playerDetails') || (not $ Array.null profileDetails'')
                    then
                        [ detailColumn $
                            ( if not $ Array.null playerDetails'
                                then [ detailColumnHeading4 "Player details" ] <> playerDetails'
                                else []
                            )
                            <>
                            ( if not $ Array.null profileDetails''
                                then [ detailColumnHeading4 "Profile details" ] <> profileDetails''
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
                                then [ detailColumnHeading4 "About" ] <> about
                                else []
                            )
                            <>
                            ( if not $ Array.null ambitions
                                then [ detailColumnHeading4 "Ambitions" ] <> ambitions
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
handleAction (OpenPreboarding mouseEvent) = do
    preventMouseDefault mouseEvent
    H.raise PreboardingClicked

component :: forall query left. H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

playerProfiles
    :: forall children action left
    .  Input
    -> (Output -> Maybe action)
    -> HH.ComponentHTML action (playerProfiles :: Slot | children) (Async left)
playerProfiles input handleOutput =
    HH.slot (SProxy :: SProxy "playerProfiles") unit component input handleOutput

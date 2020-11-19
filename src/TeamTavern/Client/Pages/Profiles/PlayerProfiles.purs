module TeamTavern.Client.Pages.Profiles.PlayerProfiles (Fields, PlayerProfile, Input, Output(..), Slot, playerProfiles) where

import Prelude

import Async (Async)
import Client.Components.Copyable as Copyable
import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Card (cardHeader, cardHeading, cardSection, cardSubheading)
import TeamTavern.Client.Components.Detail (detailColumn, detailColumnHeading, detailColumns, textDetail)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Components.Pagination (pagination)
import TeamTavern.Client.Components.Player.PlayerDetails (playerDetails)
import TeamTavern.Client.Components.Player.ProfileDetails (profileDetails')
import TeamTavern.Client.Components.Profile (profileHeader, profileHeaderItem, profileHeading, profileSubheading)
import TeamTavern.Client.Script.Cookie (PlayerInfo)
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame.LoadProfiles (pageSize)

type Fields = Array
    { ilk :: Int
    , label :: String
    , key :: String
    , icon :: String
    , required :: Boolean
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

data Output = PageChanged Int

type Slot = H.Slot (Const Void) Output Unit

type ChildSlots =
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
    ]
    <>
    if Array.null profiles
    then [ cardSection [ HH.p_ [ HH.text "No profiles satisfy specified filters." ] ] ]
    else
    ( profiles <#> \profile -> let
        playerDetails' = playerDetails profile
        profileDetails'' = profileDetails' profile.fieldValues profile.newOrReturning
        about = textDetail profile.about
        ambitions = textDetail profile.ambitions
        in
        cardSection $
        [ profileHeader $
            [ profileHeaderItem
                [ profileHeading (SProxy :: SProxy "players") profile.nickname
                    ("/players/" <> profile.nickname) profile.nickname
                , divider
                , profileSubheading $ "Updated " <> lastUpdated profile.updatedSeconds
                ]
            ]
            <>
            case playerInfo of
            Just { nickname } | nickname /= profile.nickname ->
                [ profileHeaderItem
                    [ navigationAnchorIndexed (SProxy :: SProxy "messagePlayer") profile.nickname
                        { path: "/conversations/" <> profile.nickname
                        , content: HH.span_
                            [ HH.i [ HP.class_ $ H.ClassName "fas fa-envelope button-icon" ] []
                            , HH.text "Message player"
                            ]
                        }
                    ]
                ]
            _ -> []
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
                                then [ detailColumnHeading "Player details" ] <> playerDetails'
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

playerProfiles
    :: forall children action left
    .  Input
    -> (Output -> Maybe action)
    -> HH.ComponentHTML action (playerProfiles :: Slot | children) (Async left)
playerProfiles input handleOutput =
    HH.slot (SProxy :: SProxy "playerProfiles") unit component input handleOutput

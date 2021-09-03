module TeamTavern.Client.Components.Team.ProfileFormInput (FieldValues, Input, Output, Slot, emptyInput, profileFormInput) where

import Prelude

import Async (Async)
import Data.Array (foldl)
import Data.Array as Array
import Data.Const (Const)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.MultiMap as MultiMap
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Record as Record
import Record.Extra (pick)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Input (inputErrorSublabel, inputGroup, inputGroupsHeading, inputGroupsHeading', inputRequiredSublabel, inputSublabel, responsiveInputGroups)
import TeamTavern.Client.Components.Player.PlayerInputGroup (discordTagInputGroup)
import TeamTavern.Client.Components.Player.ProfileInputGroup (platformIdInputGroup)
import TeamTavern.Client.Components.Select.MultiSelect as MultiSelect
import TeamTavern.Client.Components.Team.ProfileInputGroup (Field, Option, aboutInputGroup, fieldInputGroup, newOrReturningInputGroup)
import TeamTavern.Client.Components.Team.ProfileInputGroup as Input
import TeamTavern.Client.Components.Team.SizeInfo (sizeInfo)
import TeamTavern.Client.Components.Team.SizeInfo as SizeInfo
import TeamTavern.Client.Components.Team.TeamInputGroup (discordServerInputGroup)
import TeamTavern.Client.Pages.Profiles.TeamBadge (platformCheckboxBadges, sizeRadioBadges)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Shared.Platform (Platform(..), Platforms)
import TeamTavern.Routes.Shared.Size (Size(..))
import TeamTavern.Routes.Shared.Team (Contacts', Contacts)

type FieldValues = Array
    { fieldKey :: String
    , optionKeys :: Array String
    }

type Input =
    { details ::
        { size :: Size
        , allPlatforms :: Platforms
        , selectedPlatforms :: Array Platform
        , platformsError :: Boolean
        , fields :: Array Field
        , fieldValues :: FieldValues
        , newOrReturning :: Boolean
        , about :: String
        , aboutError :: Boolean
        }
    , contacts :: Contacts'
        ( discordTagError :: Boolean
        , discordServerError :: Boolean
        , steamIdError :: Boolean
        , riotIdError :: Boolean
        , battleTagError :: Boolean
        , psnIdError :: Boolean
        , gamerTagError :: Boolean
        , friendCodeError :: Boolean
        )
    }

type Output =
    { details ::
        { size :: Size
        , platforms :: Array Platform
        , fieldValues :: FieldValues
        , newOrReturning :: Boolean
        , about :: String
        }
    , contacts :: Contacts
    }

type State =
    { details ::
        { size :: Size
        , allPlatforms :: Platforms
        , selectedPlatforms :: Array Platform
        , platformsError :: Boolean
        , fields :: Array Field
        , fieldValues :: Input.FieldValues
        , newOrReturning :: Boolean
        , about :: String
        , aboutError :: Boolean
        }
    , contacts :: Contacts'
        ( discordTagError :: Boolean
        , discordServerError :: Boolean
        , steamIdError :: Boolean
        , riotIdError :: Boolean
        , battleTagError :: Boolean
        , psnIdError :: Boolean
        , gamerTagError :: Boolean
        , friendCodeError :: Boolean
        )
    }

data Action
    = Receive Input
    | UpdateSize Size
    | UpdatePlatform Platform
    | UpdateFieldValues String (MultiSelect.Output Option)
    | UpdateNewOrReturning Boolean
    | UpdateAbout String
    | UpdateDiscordTag (Maybe String)
    | UpdateDiscordServer (Maybe String)
    | UpdateSteamId (Maybe String)
    | UpdateRiotId (Maybe String)
    | UpdateBattleTag (Maybe String)
    | UpdatePsnId (Maybe String)
    | UpdateGamerTag (Maybe String)
    | UpdateFriendCode (Maybe String)

type Slot = H.Slot (Const Void) Output Unit

type ChildSlots =
    ( "multiSelectField" :: MultiSelect.Slot Option String
    , "sizeInfo" :: SizeInfo.Slot
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { details, contacts }
    = HH.div_ $
    [ HH.h2 [ HS.class_ "platform-id-heading" ]
        [ HH.text "Size"
        , sizeRadioBadges details.size UpdateSize
        , sizeInfo
        ]
    ]
    <> guard (not $ Array.null details.allPlatforms.tail)
    [ inputGroupsHeading'
        [ HH.text "Platforms"
        , divider, inputRequiredSublabel
        , divider, (if details.platformsError then inputErrorSublabel else inputSublabel)
            "You must select at least one of the available platforms."
        ]
    , inputGroup [ platformCheckboxBadges details.allPlatforms details.selectedPlatforms UpdatePlatform ]
    ]
    <>
    [ inputGroupsHeading' [ HH.text "Contacts", divider, inputSublabel "Contacts are shared between all your profiles." ]
    , responsiveInputGroups $
        (details.selectedPlatforms <#>
            case _ of
            Steam       -> platformIdInputGroup Steam       contacts.steamId    UpdateSteamId    contacts.steamIdError    true
            Riot        -> platformIdInputGroup Riot        contacts.riotId     UpdateRiotId     contacts.riotIdError     true
            BattleNet   -> platformIdInputGroup BattleNet   contacts.battleTag  UpdateBattleTag  contacts.battleTagError  true
            PlayStation -> platformIdInputGroup PlayStation contacts.psnId      UpdatePsnId      contacts.psnIdError      true
            Xbox        -> platformIdInputGroup Xbox        contacts.gamerTag   UpdateGamerTag   contacts.gamerTagError   true
            Switch      -> platformIdInputGroup Switch      contacts.friendCode UpdateFriendCode contacts.friendCodeError true)
        <>
        [ discordTagInputGroup contacts.discordTag UpdateDiscordTag contacts.discordTagError
        , discordServerInputGroup contacts.discordServer UpdateDiscordServer contacts.discordServerError
        ]
    ]
    <>
    [ inputGroupsHeading "Details"
    , responsiveInputGroups $
        (details.fields <#> fieldInputGroup details.fieldValues UpdateFieldValues)
        <>
        [ newOrReturningInputGroup details.newOrReturning UpdateNewOrReturning ]
    , inputGroupsHeading "About"
    , aboutInputGroup details.about UpdateAbout details.aboutError
    ]

fieldValuesToArray :: Input.FieldValues -> FieldValues
fieldValuesToArray = (MultiMap.toUnfoldable' :: _ -> Array (Tuple _ (Array _))) >>>
    map \(Tuple fieldKey optionKeys) -> { fieldKey, optionKeys }

fieldValuesToMap :: FieldValues -> Input.FieldValues
fieldValuesToMap =
    foldl
    (\fieldValues { fieldKey, optionKeys } ->
        case NonEmptyList.fromFoldable optionKeys of
        Nothing -> fieldValues
        Just optionKeys' -> MultiMap.insertOrReplace fieldKey optionKeys' fieldValues
    )
    MultiMap.empty

raiseOutput :: forall left. State -> H.HalogenM State Action ChildSlots Output (Async left) Unit
raiseOutput state = let
    details = state.details { fieldValues = fieldValuesToArray state.details.fieldValues }
        # Record.insert (SProxy :: _ "platforms") state.details.selectedPlatforms
        # pick
    contacts = pick state.contacts
    in
    H.raise { details, contacts }

handleAction :: forall left. Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive input) =
    H.put $ input { details { fieldValues = fieldValuesToMap input.details.fieldValues } }
handleAction (UpdateSize size) = H.modify _ { details { size = size } } >>= raiseOutput
handleAction (UpdatePlatform platform) = do
    state <- H.modify \state -> state
        { details
            { selectedPlatforms =
                if Array.elem platform state.details.selectedPlatforms
                then Array.delete platform state.details.selectedPlatforms
                else Array.cons platform state.details.selectedPlatforms
            }
        }
    raiseOutput state
handleAction (UpdateFieldValues fieldKey options) = do
    state <- H.modify \state -> state
        { details
            { fieldValues =
                case NonEmptyList.fromFoldable options of
                Nothing -> MultiMap.delete fieldKey state.details.fieldValues
                Just options' -> MultiMap.insertOrReplace fieldKey (_.key <$> options') state.details.fieldValues
            }
        }
    raiseOutput state
handleAction (UpdateNewOrReturning newOrReturning) = H.modify _ { details { newOrReturning = newOrReturning } } >>= raiseOutput
handleAction (UpdateAbout about) = H.modify _ { details { about = about } } >>= raiseOutput
handleAction (UpdateDiscordTag discordTag) = H.modify _ { contacts { discordTag = discordTag } } >>= raiseOutput
handleAction (UpdateDiscordServer discordServer) = H.modify _ { contacts { discordServer = discordServer } } >>= raiseOutput
handleAction (UpdateSteamId steamId)       = H.modify _ { contacts { steamId    = steamId    } } >>= raiseOutput
handleAction (UpdateRiotId riotId)         = H.modify _ { contacts { riotId     = riotId     } } >>= raiseOutput
handleAction (UpdateBattleTag battleTag)   = H.modify _ { contacts { battleTag  = battleTag  } } >>= raiseOutput
handleAction (UpdatePsnId psnId)           = H.modify _ { contacts { psnId      = psnId      } } >>= raiseOutput
handleAction (UpdateGamerTag gamerTag)     = H.modify _ { contacts { gamerTag   = gamerTag   } } >>= raiseOutput
handleAction (UpdateFriendCode friendCode) = H.modify _ { contacts { friendCode = friendCode } } >>= raiseOutput

component :: forall query left. H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: \input -> input { details { fieldValues = fieldValuesToMap input.details.fieldValues } }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

emptyInput :: { platforms :: Platforms, fields :: Array Field } -> Input
emptyInput { platforms, fields } =
    { details:
        { size: Party
        , allPlatforms: platforms
        , selectedPlatforms: [ platforms.head ]
        , platformsError: false
        , fields
        , fieldValues: []
        , newOrReturning: false
        , about: ""
        , aboutError: false
        }
    , contacts:
        { discordTag: Nothing
        , discordTagError: false
        , discordServer: Nothing
        , discordServerError: false
        , steamId: Nothing
        , steamIdError: false
        , riotId: Nothing
        , riotIdError: false
        , battleTag: Nothing
        , battleTagError: false
        , psnId: Nothing
        , psnIdError: false
        , gamerTag: Nothing
        , gamerTagError: false
        , friendCode: Nothing
        , friendCodeError: false
        }
    }

profileFormInput
    :: forall children action left
    .  Input
    -> (Output -> action)
    -> HH.ComponentHTML action (teamProfileFormInput :: Slot | children) (Async left)
profileFormInput input handleMessage =
    HH.slot (SProxy :: SProxy "teamProfileFormInput") unit component input (Just <<< handleMessage)

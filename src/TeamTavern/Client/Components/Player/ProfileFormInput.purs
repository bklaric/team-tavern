module TeamTavern.Client.Components.Player.ProfileFormInput (FieldValues, Input, Output, Slot, emptyInput, profileFormInput) where

import Prelude

import Async (Async)
import Data.Array (foldl)
import Data.Array as Array
import Data.Const (Const)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Variant (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Record as Record
import Record.Extra (pick)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Input (inputGroup, inputGroupsHeading, inputGroupsHeading', inputSublabel, responsiveInputGroups)
import TeamTavern.Client.Components.Player.PlayerInputGroup (discordTagInputGroup)
import TeamTavern.Client.Components.Player.ProfileInputGroup (ChildSlots, Field, FieldValue, aboutInputGroup, fieldInputGroup, newOrReturningInputGroup, platformIdInputGroup)
import TeamTavern.Client.Components.Player.ProfileInputGroup as Input
import TeamTavern.Client.Pages.Profiles.TeamBadge (platformRadioBadges)
import TeamTavern.Routes.Shared.Platform (Platform(..), Platforms)
import TeamTavern.Routes.Shared.Player as Routes

type FieldValues = Array FieldValue

type Input =
    { platforms :: Platforms
    , fields :: Array Field
    , platform :: Platform
    , contacts :: Routes.Contacts'
        ( discordTagError :: Boolean
        , steamIdError :: Boolean
        , riotIdError :: Boolean
        , battleTagError :: Boolean
        , psnIdError :: Boolean
        , gamerTagError :: Boolean
        , friendCodeError :: Boolean
        )
    , fieldValues :: FieldValues
    , newOrReturning :: Boolean
    , about :: String
    , urlErrors :: Array String
    , aboutError :: Boolean
    }

type Output =
    { platform :: Platform
    , contacts :: Routes.Contacts
    , fieldValues :: FieldValues
    , about :: String
    , newOrReturning :: Boolean
    }

type State =
    { platforms :: Platforms
    , fields :: Array Field
    , platform :: Platform
    , contacts :: Routes.Contacts'
        ( discordTagError :: Boolean
        , steamIdError :: Boolean
        , riotIdError :: Boolean
        , battleTagError :: Boolean
        , psnIdError :: Boolean
        , gamerTagError :: Boolean
        , friendCodeError :: Boolean
        )
    , fieldValues :: Input.FieldValues
    , newOrReturning :: Boolean
    , about :: String
    , urlErrors :: Array String
    , aboutError :: Boolean
    }

data Action
    = Receive Input
    | UpdatePlatform Platform
    | UpdateUrl String (Maybe String)
    | UpdateSingleSelect String (Maybe String)
    | UpdateMultiSelect String (Array String)
    | UpdateNewOrReturning Boolean
    | UpdateAbout String
    | UpdateDiscordTag (Maybe String)
    | UpdateSteamId (Maybe String)
    | UpdateRiotId (Maybe String)
    | UpdateBattleTag (Maybe String)
    | UpdatePsnId (Maybe String)
    | UpdateGamerTag (Maybe String)
    | UpdateFriendCode (Maybe String)

type Slot = H.Slot (Const Void) Output Unit

fieldValuesToArray :: forall key value. Map key value -> Array value
fieldValuesToArray = Array.fromFoldable <<< Map.values

fieldValuesToMap :: forall fields.
    Array { fieldKey :: String | fields } -> Map String { fieldKey :: String | fields }
fieldValuesToMap = foldl (\map value -> Map.insert value.fieldKey value map) Map.empty

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render
    { platforms, fields
    , platform, contacts, fieldValues, newOrReturning, about
    , urlErrors, aboutError
    }
    = HH.div_ $
    guard (not $ Array.null platforms.tail)
    [ inputGroupsHeading "Platform"
    , inputGroup [ platformRadioBadges platforms platform UpdatePlatform ]
    ]
    <>
    [ inputGroupsHeading' [ HH.text "Contacts", divider, inputSublabel "Contacts are shared between all your profiles." ]
    , responsiveInputGroups
        [ case platform of
            Steam       -> platformIdInputGroup Steam       contacts.steamId    UpdateSteamId    contacts.steamIdError    true
            Riot        -> platformIdInputGroup Riot        contacts.riotId     UpdateRiotId     contacts.riotIdError     true
            BattleNet   -> platformIdInputGroup BattleNet   contacts.battleTag  UpdateBattleTag  contacts.battleTagError  true
            PlayStation -> platformIdInputGroup PlayStation contacts.psnId      UpdatePsnId      contacts.psnIdError      true
            Xbox        -> platformIdInputGroup Xbox        contacts.gamerTag   UpdateGamerTag   contacts.gamerTagError   true
            Switch      -> platformIdInputGroup Switch      contacts.friendCode UpdateFriendCode contacts.friendCodeError true
        , discordTagInputGroup contacts.discordTag UpdateDiscordTag contacts.discordTagError
        ]
    ]
    <>
    [ inputGroupsHeading "Details"
    , responsiveInputGroups $
        ( fields <#> fieldInputGroup fieldValues
            UpdateUrl UpdateSingleSelect UpdateMultiSelect urlErrors
        )
        <>
        [ newOrReturningInputGroup newOrReturning UpdateNewOrReturning ]
    , inputGroupsHeading "About"
    , aboutInputGroup about UpdateAbout aboutError
    ]

raiseOutput :: forall left. State -> H.HalogenM State Action ChildSlots Output (Async left) Unit
raiseOutput { platform, contacts, fieldValues, newOrReturning, about } =
    H.raise { platform, contacts: pick contacts, fieldValues: fieldValuesToArray fieldValues, newOrReturning, about }

handleAction :: forall left. Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive input) =
    H.put (Record.modify (SProxy :: SProxy "fieldValues") fieldValuesToMap input)
handleAction (UpdatePlatform platform) = H.modify _ { platform = platform } >>= raiseOutput
handleAction (UpdateUrl fieldKey url) = do
    state <- H.modify \state -> state
        { fieldValues =
            case url of
            Nothing -> Map.delete fieldKey state.fieldValues
            Just url' ->
                Map.insert
                fieldKey
                { fieldKey, url: Just url', optionKey: Nothing, optionKeys: Nothing }
                state.fieldValues
        }
    raiseOutput state
handleAction (UpdateSingleSelect fieldKey optionKey) = do
    state <- H.modify \state -> state
        { fieldValues =
            case optionKey of
            Nothing -> Map.delete fieldKey state.fieldValues
            Just optionKey' ->
                Map.insert
                fieldKey
                { fieldKey, url: Nothing, optionKey: Just optionKey', optionKeys: Nothing }
                state.fieldValues
        }
    raiseOutput state
handleAction (UpdateMultiSelect fieldKey optionKeys) = do
    state <- H.modify \state -> state
        { fieldValues =
            case Array.uncons optionKeys of
            Nothing -> Map.delete fieldKey state.fieldValues
            Just { head, tail } ->
                Map.insert
                fieldKey
                { fieldKey
                , url: Nothing
                , optionKey: Nothing
                , optionKeys: Just $ Array.cons head tail
                }
                state.fieldValues
        }
    raiseOutput state
handleAction (UpdateNewOrReturning newOrReturning) = H.modify _ { newOrReturning = newOrReturning } >>= raiseOutput
handleAction (UpdateAbout about)                   = H.modify _ { about          = about          } >>= raiseOutput
handleAction (UpdateDiscordTag discordTag) = H.modify _ { contacts { discordTag = discordTag } } >>= raiseOutput
handleAction (UpdateSteamId steamId)       = H.modify _ { contacts { steamId    = steamId    } } >>= raiseOutput
handleAction (UpdateRiotId riotId)         = H.modify _ { contacts { riotId     = riotId     } } >>= raiseOutput
handleAction (UpdateBattleTag battleTag)   = H.modify _ { contacts { battleTag  = battleTag  } } >>= raiseOutput
handleAction (UpdatePsnId psnId)           = H.modify _ { contacts { psnId      = psnId      } } >>= raiseOutput
handleAction (UpdateGamerTag gamerTag)     = H.modify _ { contacts { gamerTag   = gamerTag   } } >>= raiseOutput
handleAction (UpdateFriendCode friendCode) = H.modify _ { contacts { friendCode = friendCode } } >>= raiseOutput

component :: forall query left. H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: Record.modify (SProxy :: SProxy "fieldValues") fieldValuesToMap
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

emptyInput :: forall props.
    { platforms :: Platforms, fields :: Array Field | props } -> Input
emptyInput { platforms, fields } =
    { platforms
    , fields
    , platform: platforms.head
    , contacts:
        { discordTag: Nothing
        , discordTagError: false
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
    , fieldValues: []
    , newOrReturning: false
    , about: ""
    , urlErrors: []
    , aboutError: false
    }

profileFormInput
    :: forall action children left
    .  Input
    -> (Output -> action)
    -> HH.ComponentHTML action (playerProfileFormInput :: Slot | children) (Async left)
profileFormInput input handleMessage =
    HH.slot (SProxy :: SProxy "playerProfileFormInput") unit component input (Just <<< handleMessage)

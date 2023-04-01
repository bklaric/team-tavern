module TeamTavern.Client.Components.Player.ProfileFormInput (Input, Output, Slot, emptyInput, profileFormInput) where

import Prelude

import Async (Async)
import Data.Array (foldl)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Halogen as H
import Halogen.HTML as HH
import Record.Extra (pick)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Input (inputGroup, inputGroupsHeading, inputGroupsHeading', inputSublabel, responsiveInputGroups)
import TeamTavern.Client.Components.Player.PlayerInputGroup (discordTagInputGroup)
import TeamTavern.Client.Components.Player.ProfileInputGroup (ChildSlots, aboutInputGroup, ambitionsInputGroup, fieldInputGroup, newOrReturningInputGroup, platformIdInputGroup)
import TeamTavern.Client.Components.Player.ProfileInputGroup as Input
import TeamTavern.Client.Pages.Profiles.TeamBadge (platformRadioBadges)
import TeamTavern.Client.Shared.Slot (Slot_O_)
import TeamTavern.Routes.Shared.Field (Fields, ValuesSimple, ValueSimple)
import TeamTavern.Routes.Shared.Platform (Platform(..), Platforms)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContacts, PlayerContactsOpen)
import Type.Proxy (Proxy(..))

type Input =
    { details ::
        { platforms :: Platforms
        , fields :: Fields
        , platform :: Platform
        , fieldValues :: ValuesSimple
        , newOrReturning :: Boolean
        , about :: String
        , ambitions :: String
        , aboutError :: Boolean
        , ambitionsError :: Boolean
        }
    , contacts :: PlayerContactsOpen
        ( discordTagError :: Boolean
        , steamIdError :: Boolean
        , riotIdError :: Boolean
        , battleTagError :: Boolean
        , eaIdError :: Boolean
        , ubisoftUsernameError :: Boolean
        , psnIdError :: Boolean
        , gamerTagError :: Boolean
        , friendCodeError :: Boolean
        )
    }

type Output =
    { details ::
        { platform :: Platform
        , fieldValues :: ValuesSimple
        , about :: String
        , ambitions :: String
        , newOrReturning :: Boolean
        }
    , contacts :: PlayerContacts
    }

type State =
    { details ::
        { platforms :: Platforms
        , fields :: Fields
        , platform :: Platform
        , fieldValues :: Input.FieldValues
        , newOrReturning :: Boolean
        , about :: String
        , ambitions :: String
        , aboutError :: Boolean
        , ambitionsError :: Boolean
        }
    , contacts :: PlayerContactsOpen
        ( discordTagError :: Boolean
        , steamIdError :: Boolean
        , riotIdError :: Boolean
        , battleTagError :: Boolean
        , eaIdError :: Boolean
        , ubisoftUsernameError :: Boolean
        , psnIdError :: Boolean
        , gamerTagError :: Boolean
        , friendCodeError :: Boolean
        )
    }

data Action
    = Receive Input
    | UpdatePlatform Platform
    | UpdateSingleSelect String (Maybe String)
    | UpdateMultiSelect String (Array String)
    | UpdateNewOrReturning Boolean
    | UpdateAbout String
    | UpdateAmbitions String
    | UpdateDiscordTag (Maybe String)
    | UpdateSteamId (Maybe String)
    | UpdateRiotId (Maybe String)
    | UpdateBattleTag (Maybe String)
    | UpdateEaId (Maybe String)
    | UpdateUbisoftUsername (Maybe String)
    | UpdatePsnId (Maybe String)
    | UpdateGamerTag (Maybe String)
    | UpdateFriendCode (Maybe String)

type Slot = Slot_O_ Output

fieldValuesToArray :: ∀ key value. Map key value -> Array value
fieldValuesToArray = Array.fromFoldable <<< Map.values

fieldValuesToMap :: ValuesSimple -> Map String ValueSimple
fieldValuesToMap = foldl (\map value -> Map.insert value.fieldKey value map) Map.empty

render :: ∀ left. State -> H.ComponentHTML Action ChildSlots (Async left)
render
    { details: { platforms, fields, platform, fieldValues, newOrReturning, about, ambitions, aboutError, ambitionsError }
    , contacts
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
            Steam       -> platformIdInputGroup Steam       contacts.steamId         UpdateSteamId         contacts.steamIdError         true
            Riot        -> platformIdInputGroup Riot        contacts.riotId          UpdateRiotId          contacts.riotIdError          true
            BattleNet   -> platformIdInputGroup BattleNet   contacts.battleTag       UpdateBattleTag       contacts.battleTagError       true
            Origin      -> platformIdInputGroup Origin      contacts.eaId            UpdateEaId            contacts.eaIdError            true
            Ubisoft     -> platformIdInputGroup Ubisoft     contacts.ubisoftUsername UpdateUbisoftUsername contacts.ubisoftUsernameError true
            PlayStation -> platformIdInputGroup PlayStation contacts.psnId           UpdatePsnId           contacts.psnIdError           true
            Xbox        -> platformIdInputGroup Xbox        contacts.gamerTag        UpdateGamerTag        contacts.gamerTagError        true
            Switch      -> platformIdInputGroup Switch      contacts.friendCode      UpdateFriendCode      contacts.friendCodeError      true
        , discordTagInputGroup contacts.discordTag UpdateDiscordTag contacts.discordTagError
        ]
    ]
    <>
    [ inputGroupsHeading "Details"
    , responsiveInputGroups $
        (fields <#> fieldInputGroup fieldValues UpdateSingleSelect UpdateMultiSelect)
        <>
        [ newOrReturningInputGroup newOrReturning UpdateNewOrReturning ]
    , inputGroupsHeading' [ HH.text "About", divider, inputSublabel "Write a bit about yourself. What are you like? What are you looking for in other players?" ]
    , aboutInputGroup about UpdateAbout aboutError
    , inputGroupsHeading' [ HH.text "Ambitions", divider, inputSublabel "What do you want to get out of playing in a team? Any specific goals you want to achieve?" ]
    , ambitionsInputGroup ambitions UpdateAmbitions ambitionsError
    ]

raiseOutput :: ∀ left. State -> H.HalogenM State Action ChildSlots Output (Async left) Unit
raiseOutput state = let
    details = state.details { fieldValues = fieldValuesToArray state.details.fieldValues } # pick
    contacts = pick state.contacts
    in
    H.raise { details, contacts }

handleAction :: ∀ left. Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive input) =
    H.put $ input { details { fieldValues = fieldValuesToMap input.details.fieldValues } }
handleAction (UpdatePlatform platform) = H.modify _ { details { platform = platform } } >>= raiseOutput
handleAction (UpdateSingleSelect fieldKey optionKey) = do
    state <- H.modify \state -> state
        { details
            { fieldValues =
                case optionKey of
                Nothing -> Map.delete fieldKey state.details.fieldValues
                Just optionKey' ->
                    Map.insert
                    fieldKey
                    { fieldKey, optionKey: Just optionKey', optionKeys: Nothing }
                    state.details.fieldValues
            }
        }
    raiseOutput state
handleAction (UpdateMultiSelect fieldKey optionKeys) = do
    state <- H.modify \state -> state
        { details
            { fieldValues =
                case Array.uncons optionKeys of
                Nothing -> Map.delete fieldKey state.details.fieldValues
                Just { head, tail } ->
                    Map.insert
                    fieldKey
                    { fieldKey
                    , optionKey: Nothing
                    , optionKeys: Just $ Array.cons head tail
                    }
                    state.details.fieldValues
            }
        }
    raiseOutput state
handleAction (UpdateNewOrReturning newOrReturning) = H.modify _ { details { newOrReturning = newOrReturning } } >>= raiseOutput
handleAction (UpdateAbout about)                   = H.modify _ { details { about          = about          } } >>= raiseOutput
handleAction (UpdateAmbitions ambitions)           = H.modify _ { details { ambitions      = ambitions      } } >>= raiseOutput
handleAction (UpdateDiscordTag discordTag)    = H.modify _ { contacts { discordTag      = discordTag } } >>= raiseOutput
handleAction (UpdateSteamId steamId)          = H.modify _ { contacts { steamId         = steamId    } } >>= raiseOutput
handleAction (UpdateRiotId riotId)            = H.modify _ { contacts { riotId          = riotId     } } >>= raiseOutput
handleAction (UpdateBattleTag battleTag)      = H.modify _ { contacts { battleTag       = battleTag  } } >>= raiseOutput
handleAction (UpdateEaId eaId)                = H.modify _ { contacts { eaId            = eaId       } } >>= raiseOutput
handleAction (UpdateUbisoftUsername username) = H.modify _ { contacts { ubisoftUsername = username   } } >>= raiseOutput
handleAction (UpdatePsnId psnId)              = H.modify _ { contacts { psnId           = psnId      } } >>= raiseOutput
handleAction (UpdateGamerTag gamerTag)        = H.modify _ { contacts { gamerTag        = gamerTag   } } >>= raiseOutput
handleAction (UpdateFriendCode friendCode)    = H.modify _ { contacts { friendCode      = friendCode } } >>= raiseOutput

component :: ∀ query left. H.Component query Input Output (Async left)
component = H.mkComponent
    { initialState: \input -> input { details { fieldValues = fieldValuesToMap input.details.fieldValues } }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

emptyInput :: ∀ props.
    { platforms :: Platforms, fields :: Fields | props } -> Input
emptyInput { platforms, fields } =
    { details:
        { platforms
        , fields
        , platform: platforms.head
        , fieldValues: []
        , newOrReturning: false
        , about: ""
        , aboutError: false
        , ambitions: ""
        , ambitionsError: false
        }
    , contacts:
        { discordTag: Nothing
        , discordTagError: false
        , steamId: Nothing
        , steamIdError: false
        , riotId: Nothing
        , riotIdError: false
        , battleTag: Nothing
        , battleTagError: false
        , eaId: Nothing
        , eaIdError: false
        , ubisoftUsername: Nothing
        , ubisoftUsernameError: false
        , psnId: Nothing
        , psnIdError: false
        , gamerTag: Nothing
        , gamerTagError: false
        , friendCode: Nothing
        , friendCodeError: false
        }
    }

profileFormInput
    :: ∀ action slots left
    .  Input
    -> (Output -> action)
    -> HH.ComponentHTML action (playerProfileFormInput :: Slot | slots) (Async left)
profileFormInput input handleMessage =
    HH.slot (Proxy :: _ "playerProfileFormInput") unit component input handleMessage

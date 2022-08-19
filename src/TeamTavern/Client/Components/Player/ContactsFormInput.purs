module TeamTavern.Client.Components.Player.ContactsFormInput (Input, Output, Slot, emptyInput, contactsFormInput) where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Type.Proxy (Proxy(..))
import Halogen as H
import Halogen.HTML as HH
import Record.Extra (pick)
import TeamTavern.Client.Components.Input (inputGroupsHeading, responsiveInputGroups)
import TeamTavern.Client.Components.Player.PlayerInputGroup (discordTagInputGroup)
import TeamTavern.Client.Components.Player.ProfileInputGroup (ChildSlots, platformIdInputGroup)
import TeamTavern.Routes.Shared.Platform (Platform(..))
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContacts, PlayerContactsOpen)

type Input = PlayerContactsOpen
    ( requiredPlatforms :: Array Platform
    , discordTagError :: Boolean
    , steamIdError :: Boolean
    , riotIdError :: Boolean
    , battleTagError :: Boolean
    , eaIdError :: Boolean
    , ubisoftUsernameError :: Boolean
    , psnIdError :: Boolean
    , gamerTagError :: Boolean
    , friendCodeError :: Boolean
    )

type Output = PlayerContacts

type State = Input

data Action
    = Receive Input
    | UpdateDiscordTag (Maybe String)
    | UpdateSteamId (Maybe String)
    | UpdateRiotId (Maybe String)
    | UpdateBattleTag (Maybe String)
    | UpdateEaId (Maybe String)
    | UpdatePsnId (Maybe String)
    | UpdateGamerTag (Maybe String)
    | UpdateFriendCode (Maybe String)

type Slot = H.Slot (Const Void) Output Unit

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render
    { requiredPlatforms
    , discordTag, discordTagError
    , steamId, steamIdError
    , riotId, riotIdError
    , battleTag, battleTagError
    , eaId, eaIdError
    , psnId, psnIdError
    , gamerTag, gamerTagError
    , friendCode, friendCodeError
    }
    = HH.div_ $
    [ inputGroupsHeading "Required"
    , responsiveInputGroups $ join
        [ guard (Steam       `elem` requiredPlatforms) [ platformIdInputGroup Steam       steamId    UpdateSteamId    steamIdError    true ]
        , guard (Riot        `elem` requiredPlatforms) [ platformIdInputGroup Riot        riotId     UpdateRiotId     riotIdError     true ]
        , guard (BattleNet   `elem` requiredPlatforms) [ platformIdInputGroup BattleNet   battleTag  UpdateBattleTag  battleTagError  true ]
        , guard (Origin      `elem` requiredPlatforms) [ platformIdInputGroup Origin      eaId       UpdateEaId       eaIdError       true ]
        , guard (PlayStation `elem` requiredPlatforms) [ platformIdInputGroup PlayStation psnId      UpdatePsnId      psnIdError      true ]
        , guard (Xbox        `elem` requiredPlatforms) [ platformIdInputGroup Xbox        gamerTag   UpdateGamerTag   gamerTagError   true ]
        , guard (Switch      `elem` requiredPlatforms) [ platformIdInputGroup Switch      friendCode UpdateFriendCode friendCodeError true ]
        ]
    , inputGroupsHeading "Discord"
    , responsiveInputGroups
        [ discordTagInputGroup discordTag UpdateDiscordTag discordTagError ]
    , inputGroupsHeading "Other"
    , responsiveInputGroups $ join
        [ guard (Steam       `not <<< elem` requiredPlatforms) [ platformIdInputGroup Steam       steamId    UpdateSteamId    steamIdError    false ]
        , guard (Riot        `not <<< elem` requiredPlatforms) [ platformIdInputGroup Riot        riotId     UpdateRiotId     riotIdError     false ]
        , guard (BattleNet   `not <<< elem` requiredPlatforms) [ platformIdInputGroup BattleNet   battleTag  UpdateBattleTag  battleTagError  false ]
        , guard (Origin      `not <<< elem` requiredPlatforms) [ platformIdInputGroup Origin      eaId       UpdateEaId       eaIdError       false ]
        , guard (PlayStation `not <<< elem` requiredPlatforms) [ platformIdInputGroup PlayStation psnId      UpdatePsnId      psnIdError      false ]
        , guard (Xbox        `not <<< elem` requiredPlatforms) [ platformIdInputGroup Xbox        gamerTag   UpdateGamerTag   gamerTagError   false ]
        , guard (Switch      `not <<< elem` requiredPlatforms) [ platformIdInputGroup Switch      friendCode UpdateFriendCode friendCodeError false ]
        ]
    ]

raiseOutput :: forall left. State -> H.HalogenM State Action ChildSlots Output (Async left) Unit
raiseOutput state = H.raise $ pick state

handleAction :: forall left. Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive input) = H.put input
handleAction (UpdateDiscordTag discordTag) = H.modify _ { discordTag = discordTag } >>= raiseOutput
handleAction (UpdateSteamId steamId)       = H.modify _ { steamId    = steamId }    >>= raiseOutput
handleAction (UpdateRiotId riotId)         = H.modify _ { riotId     = riotId }     >>= raiseOutput
handleAction (UpdateBattleTag battleTag)   = H.modify _ { battleTag  = battleTag }  >>= raiseOutput
handleAction (UpdateEaId eaId)             = H.modify _ { eaId       = eaId }       >>= raiseOutput
handleAction (UpdatePsnId psnId)           = H.modify _ { psnId      = psnId }      >>= raiseOutput
handleAction (UpdateGamerTag gamerTag)     = H.modify _ { gamerTag   = gamerTag }   >>= raiseOutput
handleAction (UpdateFriendCode friendCode) = H.modify _ { friendCode = friendCode } >>= raiseOutput

component :: forall query left. H.Component query Input Output (Async left)
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

emptyInput :: Array Platform -> Input
emptyInput requiredPlatforms =
    { requiredPlatforms
    , discordTag: Nothing
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

contactsFormInput
    :: forall action children left
    .  Input
    -> (Output -> action)
    -> HH.ComponentHTML action (playerContactsFormInput :: Slot | children) (Async left)
contactsFormInput input handleOutput =
    HH.slot (Proxy :: _ "playerContactsFormInput") unit component input handleOutput

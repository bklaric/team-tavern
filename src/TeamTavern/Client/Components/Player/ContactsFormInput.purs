module TeamTavern.Client.Components.Player.ContactsFormInput (Input, Output, Slot, emptyInput, contactsFormInput) where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Record.Extra (pick)
import TeamTavern.Client.Components.Input (responsiveInputGroups)
import TeamTavern.Client.Components.Player.PlayerInputGroup (discordTagInputGroup)
import TeamTavern.Client.Components.Player.ProfileInputGroup (ChildSlots, platformIdInputGroup)
import TeamTavern.Routes.Shared.Platform (Platform(..))

type Input =
    { requiredPlatforms :: Array Platform
    , discordTag :: Maybe String
    , discordTagError :: Boolean
    , steamId :: Maybe String
    , steamIdError :: Boolean
    , riotId :: Maybe String
    , riotIdError :: Boolean
    , battleTag :: Maybe String
    , battleTagError :: Boolean
    , psnId :: Maybe String
    , psnIdError :: Boolean
    , gamerTag :: Maybe String
    , gamerTagError :: Boolean
    , friendCode :: Maybe String
    , friendCodeError :: Boolean
    }

type Output =
    { discordTag :: Maybe String
    , steamId :: Maybe String
    , riotId :: Maybe String
    , battleTag :: Maybe String
    , psnId :: Maybe String
    , gamerTag :: Maybe String
    , friendCode :: Maybe String
    }

type State = Input

data Action
    = Receive Input
    | UpdateDiscordTag (Maybe String)
    | UpdateSteamId (Maybe String)
    | UpdateRiotId (Maybe String)
    | UpdateBattleTag (Maybe String)
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
    , psnId, psnIdError
    , gamerTag, gamerTagError
    , friendCode, friendCodeError
    }
    = HH.div_ $
    [ responsiveInputGroups
        [ discordTagInputGroup discordTag UpdateDiscordTag discordTagError
        , platformIdInputGroup Steam       steamId    UpdateSteamId    steamIdError    (elem Steam       requiredPlatforms)
        , platformIdInputGroup Riot        riotId     UpdateRiotId     riotIdError     (elem Riot        requiredPlatforms)
        , platformIdInputGroup BattleNet   battleTag  UpdateBattleTag  battleTagError  (elem BattleNet   requiredPlatforms)
        , platformIdInputGroup PlayStation psnId      UpdatePsnId      psnIdError      (elem PlayStation requiredPlatforms)
        , platformIdInputGroup Xbox        gamerTag   UpdateGamerTag   gamerTagError   (elem Xbox        requiredPlatforms)
        , platformIdInputGroup Switch      friendCode UpdateFriendCode friendCodeError (elem Switch      requiredPlatforms)
        ]
    ]

raiseOutput :: forall left. State -> H.HalogenM State Action ChildSlots Output (Async left) Unit
raiseOutput state = H.raise $ pick state

handleAction :: forall left. Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive input) = H.put input
handleAction (UpdateDiscordTag discordTag) = H.modify _ { discordTag = discordTag } >>= raiseOutput
handleAction (UpdateSteamId steamId) = H.modify _ { steamId = steamId } >>= raiseOutput
handleAction (UpdateRiotId riotId) = H.modify _ { riotId = riotId } >>= raiseOutput
handleAction (UpdateBattleTag battleTag) = H.modify _ { battleTag = battleTag } >>= raiseOutput
handleAction (UpdatePsnId psnId) = H.modify _ { psnId = psnId } >>= raiseOutput
handleAction (UpdateGamerTag gamerTag) = H.modify _ { gamerTag = gamerTag } >>= raiseOutput
handleAction (UpdateFriendCode friendCode) = H.modify _ { friendCode = friendCode } >>= raiseOutput

component :: forall query left. H.Component HH.HTML query Input Output (Async left)
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
    HH.slot (SProxy :: SProxy "playerContactsFormInput") unit component input (Just <<< handleOutput)

module TeamTavern.Client.Pages.Player.Contacts (contacts) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Monoid (guard)
import Halogen as H
import TeamTavern.Client.Components.Button (regularButton)
import TeamTavern.Client.Components.Card (card, cardHeader, cardHeading, cardSection)
import TeamTavern.Client.Components.Missing (missing)
import TeamTavern.Client.Components.Player.Contacts (ContactsSlots)
import TeamTavern.Client.Components.Player.Contacts as Components
import TeamTavern.Client.Pages.Player.Status (Status(..))
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContactsOpen)

contacts
    :: ∀ fields action slots left
    .  PlayerContactsOpen (nickname :: String | fields)
    -> Status
    -> action
    -> H.ComponentHTML action (ContactsSlots slots) (Async left)
contacts contacts' status showEditModal = let
    contacts'' = Components.contacts contacts'
    in
    card
    [ cardHeader $
        [ cardHeading "Contacts" ]
        <>
        guard (status == SignedInSelf)
        [ regularButton "fas fa-user-edit" "Edit contacts" showEditModal ]
    , cardSection
        if Array.null contacts''
        then Array.singleton $ missing
            case status of
            SignedInSelf -> "Apparently, you prefer to keep to yourself."
            _ -> "Apparently, this player prefers to keep to themself."
        else contacts''
    ]

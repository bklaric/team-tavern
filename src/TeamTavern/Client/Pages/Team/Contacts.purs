module TeamTavern.Client.Pages.Team.Contacts (contacts) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Monoid (guard)
import Halogen as H
import TeamTavern.Client.Components.Button (regularButton)
import TeamTavern.Client.Components.Card (card, cardHeader, cardHeading, cardSection)
import TeamTavern.Client.Components.Missing (missing)
import TeamTavern.Client.Components.Team.Contacts (ContactsSlots)
import TeamTavern.Client.Components.Team.Contacts as Components
import TeamTavern.Client.Pages.Team.Status (Status(..))
import TeamTavern.Routes.Shared.TeamContacts (TeamContactsOpen)

contacts
    :: ∀ fields action slots left
    .  TeamContactsOpen (handle :: String | fields)
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
        guard (status == SignedInOwner)
        [ regularButton "fas fa-user-edit" "Edit contacts" showEditModal ]
    , cardSection
        if Array.null contacts''
        then Array.singleton $ missing
            case status of
            SignedInOwner -> "Apparently, your team prefers to keep to itself."
            _ -> "Apparently, this team prefers to keep itself."
        else contacts''
    ]

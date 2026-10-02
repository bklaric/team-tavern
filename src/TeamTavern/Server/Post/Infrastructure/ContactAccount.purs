module TeamTavern.Server.Post.Infrastructure.ContactAccount (contactAccount, contactOrder) where

import Prelude

-- | SQL for the account a player holds for a `game_contact` kind, null where
-- | they hold none: the kind is an expression, the player a table alias.
contactAccount :: String -> String -> String
contactAccount player kind = """
    case """ <> kind <> """
        when 'discord' then """ <> player <> """.discord_tag
        when 'steam' then """ <> player <> """.steam_id
        when 'riot' then """ <> player <> """.riot_id
        when 'battle_tag' then """ <> player <> """.battle_tag
        when 'ea' then """ <> player <> """.ea_id
        when 'epic' then """ <> player <> """.epic_id
        when 'ubisoft' then """ <> player <> """.ubisoft_username
        when 'marvel_rivals' then """ <> player <> """.marvel_rivals_username
        when 'psn' then """ <> player <> """.psn_id
        when 'gamer_tag' then """ <> player <> """.gamer_tag
        when 'friend_code' then """ <> player <> """.friend_code
    end"""

-- | SQL for where a `game_contact` kind sorts among the others: Discord, then
-- | the publishers' and games' own accounts, then the platforms'. The account
-- | page and a post's contact panel both list contacts in this order.
contactOrder :: String -> String
contactOrder kind = """
    array_position(
        array['discord', 'riot', 'battle_tag', 'ea', 'epic', 'ubisoft', 'marvel_rivals', 'steam', 'psn', 'gamer_tag', 'friend_code'],
        """ <> kind <> """)"""

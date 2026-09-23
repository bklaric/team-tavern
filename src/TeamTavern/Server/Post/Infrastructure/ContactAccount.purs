module TeamTavern.Server.Post.Infrastructure.ContactAccount (contactAccount) where

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
        when 'ubisoft' then """ <> player <> """.ubisoft_username
        when 'psn' then """ <> player <> """.psn_id
        when 'gamer_tag' then """ <> player <> """.gamer_tag
        when 'friend_code' then """ <> player <> """.friend_code
    end"""

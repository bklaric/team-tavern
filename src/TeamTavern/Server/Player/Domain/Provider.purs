module TeamTavern.Server.Player.Domain.Provider (Provider(..), idColumn, ticketName) where

-- | A site a player signs in with in place of a password.
data Provider = Discord | Steam | Google

-- | The player column that keeps the id the provider knows the account by.
idColumn :: Provider -> String
idColumn Discord = "discord_id"
idColumn Steam = "steam_sign_in_id"
idColumn Google = "google_id"

-- | How a sign-in ticket names the provider.
ticketName :: Provider -> String
ticketName Discord = "discord"
ticketName Steam = "steam"
ticketName Google = "google"

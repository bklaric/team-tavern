# Sign-in

Status: ready-for-agent
Blocked by: 01

One sign-in identity per player, a separate contact email, Discord returning the address.

## To do

- Registration and session start work against `sign_in_identity`: email and password, or Discord. A Discord sign-in whose id is unknown registers a new player; nothing is ever linked to an existing player.
- The Discord authorize URLs in `Preboarding`, `Register` and `SignIn` request `identify email`. `FetchDiscordUser` reads `email` and `verified` from the user endpoint and the session handler stores the address as the contact email when it is verified and the player has none.
- Contact email is editable on the player's settings page and is what password reset, renewal emails and any other mail use. Password reset is only offered to email identities.
- Nickname-only players keep signing in with nickname and password; the settings page shows an empty contact email they can fill.
- Deleting a player deletes the identity and posts.

## Done when

A new Discord sign-up lands with a contact email, an existing Discord player gets one on next sign-in, and an email account and a Discord account sharing the same address are two separate players.

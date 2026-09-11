# TeamTavern

A team-finding site for gamers. A player posts that they are looking for a team, or looking for players, in a specific game, and anyone can search those posts.

## Language

### People

**Player**:
A person with a registered account. Every account belongs to one player and every post belongs to one player. A player has exactly one sign-in identity; two identities are two players.
_Avoid_: User, account

**Sign-in identity**:
The one way a player signs in: an email and password, or an account at a provider such as Discord. Unique across players and never linked to another identity.
_Avoid_: Login, credentials

**Contact email**:
The address a player's emails go to, such as post renewal reminders. Filled from the sign-in identity or its provider, editable by the player, and not unique.
_Avoid_: Email (on its own, ambiguous with the sign-in identity)

### Catalogue

**Game**:
A title in the curated catalogue that posts are made for. A game launches with no game-specific fields; rank, role and mode option lists are added only once it has posts that justify them. A rebrand renames the game in place and keeps its posts. A shut-down game is deleted together with its posts; a sequel is a new game.
_Avoid_: Title, archived game

**Platform**:
The service a player plays a game on, such as Steam, Riot or PlayStation. A post names one platform, and the player's contact for that platform is what other players use to reach them.

**Rank**, **Role**, **Mode**:
The only three kinds of game-specific field. Each is an optional list of options defined per game, at most one of each kind. On a "looking for a team" post a rank is a single value; on a "looking for players" post it is the set of ranks accepted.

### Posts

**Post**:
A player's statement, in one game, that they are looking for a team or looking for players. A player has at most one post per game per direction.
_Avoid_: Profile, listing, ad

**Looking for a team**:
The direction of a post that describes the player themself and what they want to join.
_Avoid_: Player profile, LFT

**Looking for players**:
The direction of a post that describes the group the player is recruiting for and the players it accepts. The group is described on the post; there is no team entity.
_Avoid_: Team profile, team, LFG

**Party**, **Community**:
The two sizes a "looking for players" post can name for its group. A party is a fixed handful of players wanting to fill a slot; a community is an open group such as a Discord server or organisation.

**Fresh**, **Stale**, **Archived**:
The three ages of a post. A fresh post is at most 30 days since it was last renewed and is shown first. A stale post is between 30 and 90 days, still searchable but visibly marked. An archived post is older, hidden from search, and still visible and renewable by its owner.

**Renew**:
Resetting a post's age to fresh. Editing a post renews it.
_Avoid_: Bump

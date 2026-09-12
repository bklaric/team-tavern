# TeamTavern

A team-finding site for gamers. A player publishes a profile saying they are looking for a team, or a team publishes a profile saying it is looking for players, in a specific game, and anyone can search those profiles.

## Language

### People

**Player**:
A person with a registered account. Every account belongs to one player, every player profile belongs to one player, and every team is owned by one player. A player has exactly one sign-in identity; two identities are two players.
_Avoid_: User, account

**Sign-in identity**:
The one way a player signs in: a password, entered with the player's nickname or contact email, or an account at a provider such as Discord. A player has exactly one, and it is never linked to another.
_Avoid_: Login, credentials

**Contact email**:
The address a player's emails go to, such as profile renewal reminders. Entered at registration, or filled from the provider on a Discord sign-in, and editable by the player. A password identity may be entered with it, so it is unique among players with a password and otherwise not.
_Avoid_: Email (on its own, ambiguous with the sign-in identity)

**Team**:
A named group owned by one player, with a handle and its own page, that publishes team profiles in games. A team names its size, whether it is informal or organized, an optional website and an optional Discord invite, and the contacts other players use to reach it.
_Avoid_: Group, organisation, clan

**Party**, **Community**:
The two sizes a team can be. A party is a fixed handful of players wanting to fill a slot; a community is an open group such as a Discord server or organisation.

**Informal**, **Organized**:
The two ways a team can run, independent of its size. An informal team has no membership rules or governance, such as a party climbing ranked or a lightly moderated server. An organized team has a name, membership rules such as a training schedule, and some governance, such as a team competing in leagues or a moderated community.

### Catalogue

**Game**:
A title in the curated catalogue that profiles are made for. A game enters the catalogue with the fields that a look at how it is played and who plays it justifies, which may be none, and gains or loses fields as its profiles show the need. A rebrand renames the game in place and keeps its profiles. A shut-down game is deleted together with its profiles; a sequel is a new game.
_Avoid_: Title, archived game

**Platform**:
The service a player plays a game on, such as Steam, Riot or PlayStation. A player profile names one platform and a team profile names one or more, and the contact for that platform is what other players use to reach them.

**Game field**:
A named list of options that profiles in one game fill in, such as Valorant's rank, Dota 2's server region or Valheim's server type. A game has any number of fields, and a field belongs to one game: two games with a field of the same name have two fields. Every field is either single or multi. There are no kinds of field; rank, role and mode are common names, not categories.
_Avoid_: Field kind, game-specific field

**Single**, **Multi**:
The two arities of a game field, fixed per field. A single field, such as a rank, is one option on a player profile and the set of accepted options on a team profile. A multi field, such as roles, is a set of options on both. A game may have several single fields, as ranks are often tied to a mode or a role.

### Profiles

**Player profile**:
A player's statement, in one game, that they are looking for a team: the platform they play on, their availability, the game's fields filled in for them, and a text about themself. A player has at most one per game.
_Avoid_: Post, listing, LFT

**Team profile**:
A team's statement, in one game, that it is looking for players: the platforms it plays on, the game's fields as the sets it accepts, and a text about the team. A team has at most one per game.
_Avoid_: Post, listing, LFG

**Renew**:
Setting a profile's last update to now, which moves it to the top of its listing. Editing a profile renews it, and so does the link in the renewal email a player gets once a profile has gone 30 days without an update. Renewing fires no alerts.
_Avoid_: Bump

### Alerts

**Alert**:
A saved set of search filters for one game and one of the two profile kinds, held with an email address and no account, that emails the address when a newly created profile matches. Anyone can create one from a listing page, and each email carries a link that deletes it.
_Avoid_: Saved search, subscription, notification

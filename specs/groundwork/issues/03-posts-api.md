# Posts API

Status: wontfix

Routes in `Routes/Post/` and handlers in `Server/Post/` replacing the profile routes for both directions.

## To do

- Create, update, delete, renew and view a post; view a player's posts. Update renews. Creation rejects a second post for the same player, game and direction with a typed error.
- Search by game and direction with filters: age range, locations, languages, microphone, availability, platform, new or returning, rank, role, mode. Missing availability matches any availability filter. Results sorted by renewal date, newest first, excluding archived posts, with each post carrying its age tier.
- Field arity by direction: rank is one value on a "looking for a team" post and a set on a "looking for players" post; role and mode are sets on both. Validation rejects the wrong arity.
- Group fields (size, name, Discord invite) and wanted fields (age range, locations, languages) accepted only on "looking for players" posts.
- Shared payload types in `Routes/Shared/Post.purs`; the profile, team and alert routes and handlers removed from `AllRoutes` and the server record.

## Done when

`spago build` passes with the old routes gone, and the search endpoint returns the expected posts for each filter against the migrated local database.

## Comments

Dropped with the post model. The profile routes stay as they are.

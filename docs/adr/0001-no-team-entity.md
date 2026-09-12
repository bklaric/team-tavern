---
status: deprecated
---

# No team entity; the post carries the direction

Deprecated. Designing the post model kept rediscovering the constraints that the existing player, team and profile model already encodes, and without a target UI there was no ground to prefer another model. The team entity and the two profile kinds stay as they are.

TeamTavern has no team record. A player publishes a post in a game in one of two directions, looking for a team or looking for players, and a post in the second direction describes the group on the post itself: its size, an optional name and an optional Discord invite. Sixteen months of production data showed 84% of team profiles were a party of friends filling a slot, 29% of teams were created and never given a profile because of the two-step flow, and 56 of 4180 teams used the one thing an entity offers, posts across several games. A team page or membership can be layered on later by giving such posts a shared owner record; nothing about posts has to change for that.

## Considered options

- A named team with a handle and page but no members. Rejected: nobody used the page, and it keeps the parallel table set and the extra creation step.
- Teams with membership. Rejected: no request for it in the data, and it would be the most complex part of the model for the least-used direction.
- Players only, no second direction. Rejected: searchers watching for player posts outnumbered those watching for team posts 2.5 to 1, so the recruiting side is what the search audience wants.

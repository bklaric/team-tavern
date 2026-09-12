# Ask Google to recrawl

Status: resolved

Only the site owner can do this, in Search Console.

## To do

1. Resubmit `https://www.teamtavern.net/sitemap.txt` under Sitemaps.
2. Run URL inspection on `/`, `/games`, and each of the eleven `/games/<handle>` pages. Check "View crawled page" shows listings, then "Request indexing" for each.
3. Note the date. Compare clicks and impressions for the game pages four weeks later against the week before the fix.

## Done when

The crawled-page view for a game page shows listings, and each page has been submitted for indexing.

## Comments

Done in Search Console. `sitemap.txt` is resubmitted and `/`, `/games` and all
eleven `/games/<handle>` pages have been inspected and submitted for indexing.

URL inspection found every game landing page *unindexed* rather than indexed
with empty content. That is the missing half of the traffic story: the two
faults in the spec did not just strip the listings from what Google saw, they
left the game pages out of the index altogether, which is why organic traffic
fell away after the Caddy upgrade and the move of ad insertion into the client.

Reindexing is a request, not a guarantee, so the fix is only confirmed once the
pages return to the index. Compare clicks and impressions for the game pages
four weeks from 2026-09-12 against the week before the fix.

# Ads

Status: ready-for-agent
Blocked by: 06

Placements on the new pages, with the takeover held back from the first page view.

## To do

- Record the baseline before cutover, in `specs/redesign/ads-baseline.md`: mobile Core Web Vitals from Search Console's page experience report, first-page exit rate and post-creation rate from Google Analytics, takeover impressions and revenue from the Venatus dashboard, all for the same 28 days.
- Place billboard, leaderboard, mobile MPU, vertical sticky and horizontal sticky units on the listing and player pages. The video slider is removed.
- The mobile takeover is created only on the second and later in-app navigations of a session, tracked in session storage. It is never present in server-rendered HTML.
- Ads refresh on navigation as now, except the takeover which follows the rule above.

## Done when

The first page of a fresh mobile session shows no takeover, the second does, and the baseline file exists with all four numbers.

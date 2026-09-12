# Ads

Status: ready-for-agent
Blocked by: 05

The takeover held back from the first page view, the video slider removed, and the baseline recorded.

## To do

- Record the baseline before cutover, in `specs/groundwork/ads-baseline.md`: mobile Core Web Vitals from Search Console's page experience report, first-page exit rate and profile-creation rate from Google Analytics, takeover impressions and revenue from the Venatus dashboard, all for the same 28 days.
- Remove the video slider unit. Every other placement stays where it is.
- The mobile takeover is created only on the second and later in-app navigations of a session, tracked in session storage. It is never present in server-rendered HTML.
- Ads refresh on navigation as now, except the takeover which follows the rule above.

## Done when

The first page of a fresh mobile session shows no takeover, the second does, the video slider is gone, and the baseline file exists with all four numbers.

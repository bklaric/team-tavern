# Renewal emails

Status: ready-for-agent
Blocked by: 03

Two emails per post, each with a one-click renew link, replacing the bump nobody built.

## To do

- A daily job in the Node server that finds posts whose renewal date crossed 30 days or 90 days since the previous run and sends one email per post through SendGrid to the player's contact email: at 30 days "still looking? renew", at 90 days "archived, renew to relist". Players without a contact email are skipped.
- A signed renew link, `/posts/<id>/renew?token=...`, that renews without signing in and then shows the post. Tokens are per post and invalidated by renewal or deletion.
- Sent-at columns on the post so a job that runs twice never sends twice, and so the 30 day email is not sent again after a renewal until another 30 days pass.
- Configuration for the tier boundaries in `Server/Main.purs` from environment variables, defaulting to 30 and 90.

## Done when

Against the migrated local database with the job's clock overridden, the 30 and 90 day emails go out once each per post, and following a link renews the post and resets both.

module TeamTavern.Server.Worker.SendPeriodEmails (sendPeriodEmails) where

import Prelude

import Async (Async, foreach)
import Data.Array (catMaybes, length)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import JavaScript.Date (Date)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:|))
import TeamTavern.Server.Infrastructure.Email (Block(..), Email, Mailer, sendEmail)
import TeamTavern.Server.Infrastructure.Postgres (queryMany)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Notification.Infrastructure.Visible (visibleNotification)

-- The notifications created in the period, each owner's in one row, grouped by
-- their post: a confirmed address only, fits under the Matches switch and
-- expiries under the Renewals switch (brief 11.5). A fit a block has come
-- between since is left out, as the list leaves it out.
emailsQuery :: Query
emailsQuery = Query $ """
    with news as (
        select notification.post_id, notification.kind, notification.created, notification.fitting_post_id
        from notification
        join post on post.id = notification.post_id
        join player owner on owner.id = post.player_id
        where notification.created > $1::timestamptz and notification.created <= $2::timestamptz
            and owner.email is not null and owner.email_confirmed
            and (
                notification.kind = 'fit' and owner.email_matches and """ <> visibleNotification <> """
                or notification.kind = 'expiry' and owner.email_renewals
            )
    ),
    posts as (
        select
            post.player_id,
            game.title,
            array_position(array['player', 'group', 'community'], post.ilk) as ilk_order,
            jsonb_build_object(
                'game', game.title,
                'type', post.ilk,
                'name', post.name,
                'fits', coalesce(
                    jsonb_agg(jsonb_build_object(
                        'handle', game.handle,
                        'id', fitting.id,
                        'type', fitting.ilk,
                        'name', fitting.name,
                        'owner', fitter.nickname
                    ) order by news.created, fitting.id) filter (where news.kind = 'fit'),
                    '[]'::jsonb
                ),
                'expiry', case when bool_or(news.kind = 'expiry') then jsonb_build_object(
                    'days', round(extract(epoch from post.updated + case when post.ilk = 'community'
                        then interval '90 days' else interval '30 days' end - $2::timestamptz)::numeric / 86400)::int,
                    'nonce', post.renewal_nonce,
                    'join', post.contact_preference
                ) end
            ) as post
        from news
        join post on post.id = news.post_id
        join game on game.id = post.game_id
        left join post fitting on fitting.id = news.fitting_post_id
        left join player fitter on fitter.id = fitting.player_id
        group by post.id, game.id
    )
    select owner.nickname, owner.email, jsonb_agg(posts.post order by posts.title, posts.ilk_order) as posts
    from posts
    join player owner on owner.id = posts.player_id
    group by owner.id
    """

type Fit = { handle :: String, id :: Int, type :: String, name :: Maybe String, owner :: String }

type Expiry = { days :: Int, nonce :: String, join :: String }

type OwnPost = { game :: String, type :: String, name :: Maybe String, fits :: Array Fit, expiry :: Maybe Expiry }

type Owner = { nickname :: String, email :: String, posts :: Array OwnPost }

subject :: Int -> Int -> String
subject fits expiring = let
    fitsPart = if fits == 1 then "A new post fits yours" else show fits <> " new posts fit yours"
    in
    case fits, expiring of
        0, 1 -> "Your post expires soon"
        0, _ -> show expiring <> " of your posts expire soon"
        _, 0 -> fitsPart
        _, 1 -> fitsPart <> ", and one of yours expires soon"
        _, _ -> fitsPart <> ", and " <> show expiring <> " of yours expire soon"

fitLink :: Fit -> { label :: String, path :: String }
fitLink fit =
    { label: case fit.type of
        "player" -> fit.owner <> " · player post"
        type_ -> fromMaybe fit.owner fit.name <> " · " <> type_
    , path: "/games/" <> fit.handle <> "/posts/" <> show fit.id
    }

-- A community joined through a link its owner gave, which nothing checks, has
-- its owner asked about the link before renewing.
expiryBlocks :: OwnPost -> Expiry -> Array Block
expiryBlocks post { days, nonce, join } = let
    askAbout link = Just $ Paragraph $
        "Does its " <> link <> " still work? Players join through it, and nothing checks it."
    in
    [ Just $ Paragraph case days of
        0 -> "It expires today."
        1 -> "It expires in 1 day."
        _ -> "It expires in " <> show days <> " days."
    , case post.type, join of
        "community", "discord" -> askAbout "Discord invite"
        "community", "website" -> askAbout "website link"
        _, _ -> Nothing
    , Just $ Paragraph $ "Renew it and it stays active for "
        <> (if post.type == "community" then "90" else "30") <> " days from today:"
    , Just $ Button { label: "Renew", path: "/renew?nonce=" <> nonce }
    ]
    # catMaybes

postBlocks :: OwnPost -> Array Block
postBlocks post =
    [ Heading $ "Your " <> post.game <> " " <> post.type <> maybe " post" (" " <> _) post.name ]
    <> (if length post.fits == 0 then []
        else
            [ Paragraph if length post.fits == 1 then "A new post fits it:"
                else show (length post.fits) <> " new posts fit it:"
            , Links $ fitLink <$> post.fits
            ])
    <> maybe [] (expiryBlocks post) post.expiry

periodEmail :: Owner -> Email
periodEmail owner =
    { to: owner.email
    , subject: subject
        (sum $ owner.posts <#> _.fits >>> length)
        (length $ catMaybes $ owner.posts <#> _.expiry)
    , blocks: [ Paragraph $ "Hi " <> owner.nickname <> "," ] <> (owner.posts >>= postBlocks)
    , unsubscribe: true
    }

-- | Sends each owner one email of the fits and expiries created between
-- | `since` and `now`, grouped by the post of theirs they are about (brief 8).
-- | Nothing records what was sent: a notification falls in one period.
sendPeriodEmails :: ∀ errors. Mailer -> Pool -> Date -> Date -> Async (InternalTerror_ errors) Unit
sendPeriodEmails mailer pool since now = do
    owners :: Array Owner <- queryMany pool emailsQuery (since :| now)
    foreach owners $ periodEmail >>> sendEmail mailer "Error sending period email"

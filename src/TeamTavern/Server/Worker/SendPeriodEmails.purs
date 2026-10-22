module TeamTavern.Server.Worker.SendPeriodEmails (sendPeriodEmails) where

import Prelude

import Async (Async, alwaysRightWithEffect, safeForeach)
import Data.Array (catMaybes, length)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import JavaScript.Date (Date)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import TeamTavern.Server.Infrastructure.Email (Block(..), Email, Mailer, deliver)
import TeamTavern.Server.Infrastructure.Log (logError)
import TeamTavern.Server.Infrastructure.Postgres (queryMany, queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Notification.Infrastructure.Visible (visibleNotification)
import Yoga.JSON (writeJSON)

-- Whether the owner of a notification's post takes its email, as SQL over the
-- notification, its post and the post's `owner`: a confirmed address only,
-- fits under the Matches switch and expiries under the Renewals switch
-- (brief 11.5). A fit a block has come between since is left out, as the list
-- leaves it out.
emailable :: String
emailable = """
    owner.email is not null and owner.email_confirmed
    and (
        notification.kind = 'fit' and owner.email_matches and """ <> visibleNotification <> """
        or notification.kind = 'expiry' and owner.email_renewals
    )
    """

-- A notification its owner takes no email for is let go now, so switching an
-- email on or confirming an address later sends none of what came before. One
-- whose email has failed for a day is let go too.
dropQuery :: Query
dropQuery = Query $ """
    update notification
    set email_pending = false
    from post
    join player owner on owner.id = post.player_id
    where post.id = notification.post_id and notification.email_pending
        and (notification.created <= $1::timestamptz - interval '1 day' or not (""" <> emailable <> """))
    """

-- The notifications not emailed yet, each owner's in one row, grouped by their
-- post, along with which of them the email tells of.
emailsQuery :: Query
emailsQuery = Query $ """
    with news as (
        select notification.id, notification.post_id, notification.kind, notification.created,
            notification.fitting_post_id, post.player_id as owner_id
        from notification
        join post on post.id = notification.post_id
        join player owner on owner.id = post.player_id
        where notification.email_pending and """ <> emailable <> """
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
                        then interval '90 days' else interval '30 days' end - $1::timestamptz)::numeric / 86400)::int,
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
    select owner.nickname, owner.email, jsonb_agg(posts.post order by posts.title, posts.ilk_order) as posts,
        (select jsonb_agg(jsonb_build_object('id', news.id, 'created', news.created))
            from news where news.owner_id = owner.id) as told
    from posts
    join player owner on owner.id = posts.player_id
    group by owner.id
    """

-- A notification that fired again since the email was put together, which
-- moves its created, waits for the next run.
toldQuery :: Query
toldQuery = Query """
    update notification
    set email_pending = false
    from jsonb_to_recordset($1::jsonb) as told (id integer, created timestamptz)
    where notification.id = told.id and notification.created = told.created
    """

type Fit = { handle :: String, id :: Int, type :: String, name :: Maybe String, owner :: String }

type Expiry = { days :: Int, nonce :: String, join :: String }

type OwnPost = { game :: String, type :: String, name :: Maybe String, fits :: Array Fit, expiry :: Maybe Expiry }

-- A notification as the email tells of it, with the time it fired, read back
-- as Postgres wrote it.
type Told = { id :: Int, created :: String }

type Owner = { nickname :: String, email :: String, posts :: Array OwnPost, told :: Array Told }

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

-- | Sends each owner one email of the fits and expiries not emailed yet,
-- | grouped by the post of theirs they are about (brief 8), and marks them
-- | emailed once it has gone. An owner whose email fails gets it on a later
-- | run.
sendPeriodEmails :: ∀ errors. Mailer -> Pool -> Date -> Async (InternalTerror_ errors) Unit
sendPeriodEmails mailer pool now = do
    queryNone pool dropQuery (now : [])
    owners :: Array Owner <- queryMany pool emailsQuery (now : [])
    safeForeach owners \owner ->
        do
            deliver mailer $ periodEmail owner
            queryNone pool toldQuery (writeJSON owner.told : [])
        # alwaysRightWithEffect (logError "Error sending period email") pure

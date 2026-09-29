module TeamTavern.Server.Post.UpdatePost (updatePost) where

import Prelude

import Async (Async)
import Data.Newtype (unwrap)
import Data.Nullable (toNullable)
import Effect.Class (liftEffect)
import Effect.Now (nowDate)
import Jarilo (noContent_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import TeamTavern.Routes.Shared.Post (RequestContent)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstNotFound, transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Post.Infrastructure.ClearExpiry (clearExpiry)
import TeamTavern.Server.Post.Infrastructure.LoadCatalogue (loadCatalogue)
import TeamTavern.Server.Post.Infrastructure.NotifyFits (notifyFits)
import TeamTavern.Server.Post.Infrastructure.ValidatePost (validatePost)
import TeamTavern.Server.Post.Infrastructure.WriteAccount (writeAccount)
import TeamTavern.Server.Post.Infrastructure.WriteAnswers (writeAnswers)

-- Saving renews the post (brief 9), so updated moves to now. Whether it had
-- expired is read before the update, since returning sees only the new time.
updateQuery :: Query
updateQuery = Query """
    update post
    set updated = now(),
        summary = $4::text[],
        microphone = $5,
        online_from = $6::time,
        online_to = $7::time,
        contact_preference = $8,
        name = $9,
        regions = $10::text[],
        languages = $11::text[],
        website = $12,
        discord_server = $13,
        age_from = $14::integer,
        age_to = $15::integer,
        group_size = $16::integer,
        group_wanted_from = $17::integer,
        group_wanted_to = $18::integer
    from (
        select id, updated <= now() - case when ilk = 'community'
            then interval '90 days' else interval '30 days' end as expired
        from post
        where player_id = $1 and game_id = $2 and ilk = $3
        for update
    ) saved
    where post.id = saved.id
    returning post.id, saved.expired
    """

updatePost :: ∀ left. Pool -> String -> String -> Cookies -> RequestContent -> Async left _
updatePost pool handle type_ cookies content =
    sendResponse "Error updating post" do
    { id } <- ensureSignedIn pool cookies
    { gameId, game, countries } <- loadCatalogue pool handle type_
    today <- liftEffect nowDate
    { post, summary, account } <- validatePost game countries type_ today content
    let playerId = unwrap id
    pool # transaction \client -> do
        writeAccount client playerId account
        { id: postId, expired } :: { id :: Int, expired :: Boolean } <- queryFirstNotFound client updateQuery
            ( playerId : gameId : type_ : summary : post.microphone
            : toNullable (post.online <#> _.from) : toNullable (post.online <#> _.to)
            : post.contactPreference : toNullable post.name : post.regions : post.languages
            : toNullable post.website : toNullable post.discordServer
            : toNullable post.ageFrom : toNullable post.ageTo
            : toNullable post.groupSize : toNullable post.groupWantedFrom :| toNullable post.groupWantedTo
            )
        writeAnswers client gameId postId post
        clearExpiry client postId
        -- Saving an expired post renews it, which tells the owners of the
        -- posts it fits again (brief 8), as publishing it did.
        when expired $ notifyFits client postId
    pure noContent_

module TeamTavern.Server.Post.Infrastructure.WriteAnswers (writeAnswers) where

import Prelude

import Async (Async)
import JavaScript.Npm.Pg.Client (Client)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import TeamTavern.Routes.Shared.Post (PostContent)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import Yoga.JSON (writeImpl)

clearQuery :: Query
clearQuery = Query """
    with options as (delete from post_field_option where post_id = $1),
    ranges as (delete from post_field_range where post_id = $1)
    delete from post_field_flag where post_id = $1
    """

-- $2 is { "<field>": ["<option>", ...] }.
optionsQuery :: Query
optionsQuery = Query """
    insert into post_field_option (post_id, field_option_id)
    select $1::integer, option.id
    from jsonb_each($2::jsonb) answer (key, options)
    join field on field.game_id = $3::integer and field.key = answer.key
    cross join jsonb_array_elements_text(answer.options) chosen (key)
    join field_option option on option.field_id = field.id and option.key = chosen.key
    """

-- $2 is { "<field>": { "from": "<option>", "to": "<option>" } }, either end
-- null for an open range.
rangesQuery :: Query
rangesQuery = Query """
    insert into post_field_range (post_id, field_id, from_option_id, to_option_id)
    select
        $1::integer,
        field.id,
        (select id from field_option where field_id = field.id and key = range.value->>'from'),
        (select id from field_option where field_id = field.id and key = range.value->>'to')
    from jsonb_each($2::jsonb) range
    join field on field.game_id = $3::integer and field.key = range.key
    where range.value->>'from' is not null or range.value->>'to' is not null
    """

flagsQuery :: Query
flagsQuery = Query """
    insert into post_field_flag (post_id, field_id)
    select $1::integer, field.id
    from field
    where field.game_id = $3::integer and field.key = any($2::text[])
    """

-- | Puts the post's answers in place of whatever it answered before. The keys
-- | have been validated against the game, so every one finds its row.
writeAnswers :: ∀ errors. Client -> Int -> Int -> PostContent -> Async (InternalTerror_ errors) Unit
writeAnswers client gameId postId post = do
    queryNone client clearQuery (postId : [])
    queryNone client optionsQuery (postId : writeImpl post.options :| gameId)
    queryNone client rangesQuery (postId : writeImpl post.ranges :| gameId)
    queryNone client flagsQuery (postId : post.flags :| gameId)

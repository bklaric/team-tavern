module TeamTavern.Server.Game.ViewGame (viewGame) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Jarilo (ok_)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), (:))
import TeamTavern.Routes.Game.ViewGame as ViewGame
import TeamTavern.Server.Infrastructure.Error (elaborate)
import TeamTavern.Server.Infrastructure.Postgres (LoadSingleError, queryFirstNotFound)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

queryString :: Query
queryString = Query """
    select
        game.handle,
        game.title,
        game.short_title as "shortTitle",
        json_build_object(
            'head', game.platforms[1],
            'tail', game.platforms[2:]
        ) as "platforms",
        (   select coalesce(
                json_agg(
                    json_build_object(
                        'platform', tracker.platform,
                        'title', tracker.title,
                        'template', tracker.template
                    )
                ) filter (where tracker.id is not null),
                '[]'
            )
            from tracker
            where tracker.game_id = game.id
        ) as trackers,
        coalesce(
            json_agg(
                json_build_object(
                    'ilk', field.ilk,
                    'label', field.label,
                    'key', field.key,
                    'icon', field.icon,
                    'options', field.options
                ) order by field.ordinal
            ) filter (where field.id is not null),
            '[]'
        ) as fields
    from game
        left join (
            select
                field.*,
                json_agg(
                    json_build_object(
                        'key', field_option.key,
                        'label', field_option.label
                    ) order by field_option.ordinal
                ) filter (where field_option.id is not null) as options
            from field
                left join field_option on field_option.field_id = field.id
            group by
                field.id
            ) as field on field.game_id = game.id
    where game.handle = $1
    group by game.id;
    """

loadGame :: ∀ errors. Pool -> String -> Async (LoadSingleError errors) ViewGame.OkContent
loadGame pool handle = queryFirstNotFound pool queryString (handle : [])
    # lmap (elaborate ("Can't find game: " <> handle))

viewGame :: ∀ left. Pool -> String -> Async left _
viewGame pool handle =
    sendResponse "Error viewing game" do
    -- Load game from database.
    ok_ <$> loadGame pool handle

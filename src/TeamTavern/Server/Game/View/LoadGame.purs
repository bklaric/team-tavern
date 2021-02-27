module TeamTavern.Server.Game.View.LoadGame (loadGame) where

import Async (Async)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), (:))
import TeamTavern.Routes.ViewGame as ViewGame
import TeamTavern.Server.Infrastructure.Error (LoadSingleError)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstNotFound)

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
        coalesce(
            json_agg(
                json_build_object(
                    'ilk', field.ilk,
                    'label', field.label,
                    'key', field.key,
                    'icon', field.icon,
                    'domain', field.domain,
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

loadGame :: forall errors. Pool -> String -> Async (LoadSingleError errors) ViewGame.OkContent
loadGame pool handle = queryFirstNotFound pool queryString (handle : [])

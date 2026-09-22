module TeamTavern.Server.Game.ViewGame (loadGame, viewGame) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Jarilo (ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import TeamTavern.Routes.Game.ViewGame as ViewGame
import TeamTavern.Server.Infrastructure.Error (elaborate)
import TeamTavern.Server.Infrastructure.Postgres (LoadSingleError, queryFirstNotFound)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

loadGameQuery :: Query
loadGameQuery = Query """
    select
        game.handle,
        game.title,
        game.short_title as "shortTitle",
        (   select count(*)::int
            from post
            where post.game_id = game.id
                and post.updated > now() - case when post.ilk = 'community'
                    then interval '90 days' else interval '30 days' end
        ) as active,
        array(
            select kind from game_contact
            where game_contact.game_id = game.id
            order by kind
        ) as contacts,
        coalesce((
            select jsonb_agg(jsonb_build_object(
                'contact', tracker.contact_kind,
                'title', tracker.title,
                'template', tracker.template
            ) order by tracker.id)
            from tracker
            where tracker.game_id = game.id
        ), '[]') as trackers,
        coalesce((
            select jsonb_agg(jsonb_build_object(
                'key', field.key,
                'label', field.label,
                'ilk', field.ilk,
                'ordered', field.ordered,
                'slotted', field.slotted,
                'appliesTo', field.applies_to,
                'onCard', field.on_card,
                'options', coalesce((
                    select jsonb_agg(jsonb_build_object(
                        'key', field_option.key,
                        'label', field_option.label
                    ) order by field_option.ordinal)
                    from field_option
                    where field_option.field_id = field.id
                ), '[]')
            ) order by field.ordinal)
            from field
            where field.game_id = game.id
        ), '[]') as fields
    from game
    where game.handle = $1
    """

loadGame :: ∀ errors. Pool -> String -> Async (LoadSingleError errors) ViewGame.OkContent
loadGame pool handle = queryFirstNotFound pool loadGameQuery (handle : [])
    # lmap (elaborate ("Can't find game: " <> handle))

viewGame :: ∀ left. Pool -> String -> Async left _
viewGame pool handle =
    sendResponse "Error viewing game" do
    ok_ <$> loadGame pool handle

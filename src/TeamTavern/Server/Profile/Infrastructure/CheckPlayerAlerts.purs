module TeamTavern.Server.Profile.Infrastructure.CheckPlayerAlerts where

import Prelude

import Async (Async, alwaysRightWithEffect, fromEffect, runSafeAsync, safeForeach)
import Data.Maybe (Maybe(..))
import Effect.Timer (setTimeout)
import Postgres.Query (class Querier, Query(..), (:))
import Record.Extra (pick)
import Sendgrid (sendAsync)
import TeamTavern.Routes.Shared.Organization (Organization)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.Size (Size)
import TeamTavern.Server.Infrastructure.Log (logError)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstMaybe, queryMany)
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame.LoadProfiles (createFieldsFilterString, createPlayerFilterString)

type Alert =
    { title ∷ String
    , id ∷ Int
    , email ∷ String
    , token ∷ String
    , organizations ∷ Array Organization
    , ageFrom ∷ Maybe Int
    , ageTo ∷ Maybe Int
    , locations ∷ Array String
    , languages ∷ Array String
    , microphone ∷ Boolean
    , timezone ∷ String
    , weekdayFrom ∷ Maybe String
    , weekdayTo ∷ Maybe String
    , weekendFrom ∷ Maybe String
    , weekendTo ∷ Maybe String
    , sizes ∷ Array Size
    , platforms ∷ Array Platform
    , fields ∷ Array { fieldKey ∷ String, optionKeys ∷ Array String }
    , newOrReturning ∷ Boolean
    }

loadAlertsQueryString ∷ Query
loadAlertsQueryString = Query """
    select
        game.title,
        alert.id,
        alert.email,
        alert.token,
        alert.organizations,
        alert.age_from as "ageFrom",
        alert.age_to as "ageTo",
        alert.locations,
        alert.languages,
        alert.microphone,
        alert.timezone,
        alert.weekday_from as "weekdayFrom",
        alert.weekday_to as "weekdayTo",
        alert.weekend_from as "weekendFrom",
        alert.weekend_to as "weekendTo",
        alert.sizes,
        alert.platforms,
        alert.fields,
        alert.new_or_returning as "newOrReturning"
    from alert
    join game on game.id = alert.game_id
    join player_profile on player_profile.game_id = game.id
    where player_profile.id = $1 and alert.player_or_team = 'player';
    """

checkAlertQueryString ∷ Int -> Alert -> Query
checkAlertQueryString profileId alert = Query $ """
    select profile.nickname
    from
        (select
            player.nickname,
            coalesce(
                jsonb_agg(
                    jsonb_build_object(
                        'field', jsonb_build_object(
                            'key', field_values.key
                        ),
                        case
                            when field_values.ilk = 1 then 'url'
                            when field_values.ilk = 2 then 'option'
                            when field_values.ilk = 2 then 'options'
                            when field_values.ilk = 3 then 'options'
                            when field_values.ilk = 3 then 'options'
                        end,
                        case
                            when field_values.ilk = 1 then field_values.url
                            when field_values.ilk = 2 then field_values.single
                            when field_values.ilk = 2 then field_values.multi
                            when field_values.ilk = 3 then field_values.multi
                            when field_values.ilk = 3 then field_values.multi
                        end
                    )
                ) filter (where field_values.player_profile_id is not null),
                '[]'
            ) as "fieldValues"
        from player_profile as profile
            join game on game.id = profile.game_id
            join player on player.id = profile.player_id
            left join (
                select field_value.player_profile_id,
                    field.ilk,
                    field.key,
                    to_jsonb(field_value.url) as url,
                    jsonb_build_object(
                        'key', single.key
                    ) as single,
                    coalesce(
                        jsonb_agg(
                            jsonb_build_object(
                                'key', multi.key
                            ) order by multi.ordinal
                        ) filter (where multi.label is not null),
                        '[]'
                    ) as multi
                from field
                    join player_profile_field_value as field_value on field_value.field_id = field.id
                    left join player_profile_field_value_option as field_value_option
                        on field_value_option.player_profile_field_value_id = field_value.id
                    left join field_option as single
                        on single.id = field_value.field_option_id
                    left join field_option as multi
                        on multi.id = field_value_option.field_option_id
                group by
                    field.id,
                    field_value.id,
                    single.id
            ) as field_values
                on field_values.player_profile_id = profile.id
        where
            profile.id = """ <> show profileId
            <> createPlayerFilterString alert.timezone (pick alert) <> """
        group by player.id, game.id, profile.id
        ) as profile
    """ <> createFieldsFilterString alert.fields

checkPlayerAlerts ∷ ∀ querier left. Querier querier => Int -> querier -> Async left Unit
checkPlayerAlerts profileId querier =
    fromEffect $ void $ setTimeout 0 $ runSafeAsync pure (
    alwaysRightWithEffect (logError "Error checking player alerts") pure do

    -- Load all player alerts for the game.
    (alerts ∷ Array Alert) <- queryMany querier loadAlertsQueryString (profileId : [])

    safeForeach alerts \alert -> alwaysRightWithEffect (logError "Error sending player alerts") pure do
        -- Check if alert matches the profile.
        player <- queryFirstMaybe querier
            (checkAlertQueryString profileId alert) [] ∷ _ _ (Maybe { nickname ∷ String })
        case player of
            Nothing -> pure unit
            Just { nickname } -> do
                -- Send the email.
                let playerUrl = "https://www.teamtavern.net/players/" <> nickname
                let deleteAlertUrl = "https://www.teamtavern.net/remove-alert?id=" <> show alert.id <> "&token=" <> alert.token
                sendAsync
                    { from: "admin@teamtavern.net"
                    , to: alert.email
                    , subject: "A player created a matching " <> alert.title <> " profile on TeamTavern"
                    , text: "Player " <> nickname <> " created their " <> alert.title <> " profile and it matches your alert.\n"
                        <> "You can check out their profile at: " <> playerUrl <> "\n"
                        <> "If you no longer wish to receive further emails for this alert, you can unsubscribe at " <> deleteAlertUrl
                    , html: "<p>Player " <> nickname <> " created their " <> alert.title <> " profile and it matches your alert.</p>"
                        <> "<p>You can check out their profile at: <a href=\"" <> playerUrl <> "\">" <> playerUrl <> "</a></p>"
                        <> "<p>If you no longer wish to receive further emails for this alert, you can <a href=\"" <> deleteAlertUrl <> "\">unsubscribe here</a>.</p>"
                    }
    )

module TeamTavern.Server.Profile.Infrastructure.CheckTeamAlerts where

import Prelude

import Async (Async, alwaysRightWithEffect, fromEffect, runSafeAsync, safeForeach)
import Data.Maybe (Maybe(..), maybe)
import Effect.Class.Console (log)
import Effect.Timer (setTimeout)
import Postgres.Query (class Querier, Query(..), (:))
import Record.Extra (pick)
import Sendgrid (sendAsync)
import TeamTavern.Routes.Shared.Organization (Organization)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.Size (Size)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstMaybe, queryMany)
import TeamTavern.Server.Profile.ViewTeamProfilesByGame.LoadProfiles (createFieldsFilterString, createTeamFilterString)
import Yoga.JSON (unsafeStringify)

type Alert =
    { title :: String
    , handle :: String
    , id :: Int
    , email :: String
    , token :: String
    , organizations :: Array Organization
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , timezone :: String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , sizes :: Array Size
    , platforms :: Array Platform
    , fields :: Array { fieldKey :: String, optionKeys :: Array String }
    , newOrReturning :: Boolean
    }

loadAlertsQueryString :: Query
loadAlertsQueryString = Query """
    select
        game.title,
        game.handle,
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
    join team_profile on team_profile.game_id = game.id
    where team_profile.id = $1 and alert.player_or_team = 'team';
    """

queryStringWithoutPagination :: Int -> Alert -> Query
queryStringWithoutPagination profileId alert = Query $ """
    select profile.handle, profile.name
    from
        (select
            team.handle,
            team.name,
            coalesce(
                jsonb_agg(
                    jsonb_build_object(
                        'field', jsonb_build_object(
                            'key', field_values.key
                        ),
                        'options', field_values.multi
                    )
                ) filter (where field_values.team_profile_id is not null),
                '[]'
            ) as "fieldValues"
        from team_profile as profile
            join game on game.id = profile.game_id
            join team on team.id = profile.team_id
            left join (
                select field_value.team_profile_id,
                    field.ilk,
                    field.key,
                    coalesce(
                        jsonb_agg(
                            jsonb_build_object(
                                'key', multi.key
                            ) order by multi.ordinal
                        ) filter (where multi.label is not null),
                        '[]'
                    ) as multi
                from
                    field
                    join team_profile_field_value as field_value
                        on field_value.field_id = field.id
                    left join team_profile_field_value_option as field_value_option
                        on field_value_option.team_profile_field_value_id = field_value.id
                    left join field_option as multi
                        on multi.id = field_value_option.field_option_id
                where
                    field.ilk = 2 or field.ilk = 3
                group by
                    field.id,
                    field_value.id
            ) as field_values
                on field_values.team_profile_id = profile.id
        where
            profile.id = """ <> show profileId
            <> createTeamFilterString alert.timezone (pick alert) <> """
        group by team.id, game.id, profile.id
        ) as profile
    """ <> createFieldsFilterString alert.fields

checkTeamAlerts :: ∀ querier left. Querier querier => Int -> querier -> Async left Unit
checkTeamAlerts profileId querier =
    fromEffect $ void $ setTimeout 0 $ runSafeAsync pure (
    alwaysRightWithEffect (log <<< unsafeStringify) pure do

    -- Load all player alerts for the game.
    (alerts :: Array Alert) <- queryMany querier loadAlertsQueryString (profileId : [])

    safeForeach alerts \alert -> alwaysRightWithEffect (log <<< unsafeStringify) pure do
        -- Check if alert matches the profile.
        teamMaybe :: Maybe { handle :: String, name :: Maybe String } <-
            queryFirstMaybe querier (queryStringWithoutPagination profileId alert) []
        case teamMaybe of
            Nothing -> pure unit
            Just {handle, name} -> do
                -- Send the email.
                let teamUrlShort = "https://www.teamtavern.net/teams/" <> handle <> "/" <> alert.handle
                let teamUrlLong = "https://www.teamtavern.net/teams/" <> handle <> "/" <> alert.handle <> "?id=" <> show alert.id <> "&token=" <> alert.token
                let teamName = maybe handle identity name
                let deleteAlertUrl = "https://www.teamtavern.net/remove-alert?id=" <> show alert.id <> "&token=" <> alert.token
                sendAsync
                    { from: "admin@teamtavern.net"
                    , to: alert.email
                    , subject: "A team created a matching " <> alert.title <> " profile on TeamTavern"
                    , text: "Team " <> teamName <> " created their " <> alert.title <> " profile and it matches your alert.\n"
                        <> "You can check out their profile at: " <> teamUrlLong <> "\n"
                        <> "If you no longer wish to receive further emails for this alert, you can unsubscribe at " <> deleteAlertUrl
                    , html: "<p>Team " <> teamName <> " created their " <> alert.title <> " profile and it matches your alert.</p>"
                        <> "<p>You can check out their profile at: <a href=\"" <> teamUrlLong <> "\">" <> teamUrlShort <> "</a></p>"
                        <> "<p>If you no longer wish to receive further emails for this alert, you can <a href=\"" <> deleteAlertUrl <> "\">unsubscribe here</a>.</p>"
                    }
    )

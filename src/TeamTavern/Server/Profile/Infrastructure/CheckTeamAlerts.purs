module TeamTavern.Server.Profile.Infrastructure.CheckTeamAlerts where

import Prelude

import Async (Async, alwaysRightWithEffect, fromEffect, runSafeAsync, safeForeach)
import Data.Maybe (Maybe(..), maybe)
import Effect.Timer (setTimeout)
import Postgres.Query (class Querier, Query(..), (:))
import Record.Extra (pick)
import Sendgrid (sendAsync)
import TeamTavern.Routes.Shared.Filters (Field)
import TeamTavern.Routes.Shared.Organization (Organization)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.Size (Size)
import TeamTavern.Server.Infrastructure.Log (logError)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstMaybe, queryMany)
import TeamTavern.Server.Profile.Infrastructure.LoadFieldAndOptionIds (FieldAndOptionIds, loadFieldAndOptionIds)
import TeamTavern.Server.Profile.ViewTeamProfilesByGame.LoadProfiles (createFieldsFilterString, createFieldsJoinString, createTeamFilterString)

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
    , fields :: Array Field
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

checkAlertQueryString :: Int -> Alert -> Array FieldAndOptionIds -> Query
checkAlertQueryString profileId alert fieldAndOptionIds = Query $ """
    select team.handle, team.name
    from team_profile profile
    join team on team.id = profile.team_id """
    <> createFieldsJoinString fieldAndOptionIds
    <> " where profile.id = " <> show profileId
    <> createTeamFilterString alert.timezone (pick alert)
    <> createFieldsFilterString fieldAndOptionIds
    <> " group by team.handle, team.name"

checkTeamAlerts :: ∀ querier left. Querier querier => Int -> querier -> Async left Unit
checkTeamAlerts profileId querier =
    fromEffect $ void $ setTimeout 0 $ runSafeAsync pure (
    alwaysRightWithEffect (logError "Error checking team alerts") pure do

    -- Load all player alerts for the game.
    (alerts :: Array Alert) <- queryMany querier loadAlertsQueryString (profileId : [])

    safeForeach alerts \alert -> alwaysRightWithEffect (logError "Error sending team alerts") pure do
        -- Load field and option ids.
        fieldAndOptionIds <- loadFieldAndOptionIds querier alert.handle alert.fields

        -- Check if alert matches the profile.
        teamMaybe :: Maybe { handle :: String, name :: Maybe String } <-
            queryFirstMaybe querier (checkAlertQueryString profileId alert fieldAndOptionIds) []

        case teamMaybe of
            Nothing -> pure unit
            Just {handle, name} -> do
                -- Send the email.
                let teamUrlShort = "https://www.teamtavern.net/teams/" <> handle <> "/profiles/" <> alert.handle
                let teamUrlLong = "https://www.teamtavern.net/teams/" <> handle <> "/profiles/" <> alert.handle <> "?id=" <> show alert.id <> "&token=" <> alert.token
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

module TeamTavern.Server.Profile.Infrastructure.CheckPlayerAlerts where

import Prelude

import Async (Async, alwaysRightWithEffect, fromEffect, runSafeAsync, safeForeach)
import Data.Maybe (Maybe(..))
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
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame.LoadProfiles (createFieldsFilterString, createFieldsJoinString, createPlayerFilterString)

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
    join player_profile on player_profile.game_id = game.id
    where player_profile.id = $1 and alert.player_or_team = 'player';
    """

checkAlertQueryString :: Int -> Alert -> Array FieldAndOptionIds -> Query
checkAlertQueryString profileId alert fieldAndOptionIds = Query $ """
    select player.nickname
    from player_profile profile
    join player on player.id = profile.player_id """
    <> createFieldsJoinString fieldAndOptionIds
    <> " where profile.id = " <> show profileId
    <> createPlayerFilterString alert.timezone (pick alert)
    <> createFieldsFilterString fieldAndOptionIds
    <> " group by player.nickname"

checkPlayerAlerts :: ∀ querier left. Querier querier => Int -> querier -> Async left Unit
checkPlayerAlerts profileId querier =
    fromEffect $ void $ setTimeout 0 $ runSafeAsync pure (
    alwaysRightWithEffect (logError "Error checking player alerts") pure do

    -- Load all player alerts for the game.
    (alerts :: Array Alert) <- queryMany querier loadAlertsQueryString (profileId : [])

    safeForeach alerts \alert -> alwaysRightWithEffect (logError "Error sending player alerts") pure do
        -- Load field and option ids.
        fieldAndOptionIds <- loadFieldAndOptionIds querier alert.handle alert.fields

        -- Check if alert matches the profile.
        player :: Maybe { nickname :: String } <- queryFirstMaybe querier
            (checkAlertQueryString profileId alert fieldAndOptionIds) []

        case player of
            Nothing -> pure unit
            Just { nickname } -> do
                -- Send the email.
                let playerUrlShort = "https://www.teamtavern.net/players/" <> nickname <> "/profiles/" <> alert.handle
                let playerUrlLong = "https://www.teamtavern.net/players/" <> nickname <> "/profiles/" <> alert.handle <> "?id=" <> show alert.id <> "&token=" <> alert.token
                let deleteAlertUrl = "https://www.teamtavern.net/remove-alert?id=" <> show alert.id <> "&token=" <> alert.token
                sendAsync
                    { from: "admin@teamtavern.net"
                    , to: alert.email
                    , subject: "A player created a matching " <> alert.title <> " profile on TeamTavern"
                    , text: "Player " <> nickname <> " created their " <> alert.title <> " profile and it matches your alert.\n"
                        <> "You can check out their profile at: " <> playerUrlLong <> "\n"
                        <> "If you no longer wish to receive further emails for this alert, you can unsubscribe at " <> deleteAlertUrl
                    , html: "<p>Player " <> nickname <> " created their " <> alert.title <> " profile and it matches your alert.</p>"
                        <> "<p>You can check out their profile at: <a href=\"" <> playerUrlLong <> "\">" <> playerUrlShort <> "</a></p>"
                        <> "<p>If you no longer wish to receive further emails for this alert, you can <a href=\"" <> deleteAlertUrl <> "\">unsubscribe here</a>.</p>"
                    }
    )

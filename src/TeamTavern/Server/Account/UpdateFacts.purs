module TeamTavern.Server.Account.UpdateFacts (updateFacts) where

import Prelude

import Async (Async)
import Async.Validated as AsyncVal
import Data.Bifunctor (lmap)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (toNullable)
import Data.Variant (inj)
import Effect.Class (liftEffect)
import Effect.Now (nowDate)
import Jarilo (badRequest_, internal__, noContent_)
import JavaScript.Node.Errors.Class (code)
import JavaScript.Npm.Pg.Async (query)
import JavaScript.Npm.Pg.Error (constraint)
import JavaScript.Npm.Pg.Error.Codes (unique_violation)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import TeamTavern.Routes.Account.UpdateFacts as UpdateFacts
import TeamTavern.Server.Account.Infrastructure.ValidateAccount (accountChecks, normalizedAccount)
import TeamTavern.Server.Country.ViewCountries (loadCountries)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Postgres (databaseErrorLines)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Player.Domain.Nickname (validateNickname)
import Type.Proxy (Proxy(..))
import Yoga.JSON (writeImpl)

-- Every column is written as given, so a fact left empty is cleared, unlike
-- the post screen's `writeAccount`, which only fills. $7 is
-- { "<contact kind>": "<account>" }.
queryString :: Query
queryString = Query """
    update player
    set nickname = $2,
        country = $3::text,
        languages = $4::text[],
        birthday = $5::date,
        timezone = $6::text,
        discord_tag = $7::jsonb->>'discord',
        steam_id = $7::jsonb->>'steam',
        riot_id = $7::jsonb->>'riot',
        battle_tag = $7::jsonb->>'battle_tag',
        ea_id = $7::jsonb->>'ea',
        ubisoft_username = $7::jsonb->>'ubisoft',
        psn_id = $7::jsonb->>'psn',
        gamer_tag = $7::jsonb->>'gamer_tag',
        friend_code = $7::jsonb->>'friend_code'
    where id = $1
    """

allContactKinds :: Array String
allContactKinds = [ "discord", "steam", "riot", "battle_tag", "ea", "ubisoft", "psn", "gamer_tag", "friend_code" ]

updateFacts :: ∀ left. Pool -> Cookies -> UpdateFacts.RequestContent -> Async left _
updateFacts pool cookies { nickname, account } =
    sendResponse "Error updating account facts" do
    { id } <- ensureSignedIn pool cookies
    countries <- loadCountries pool
    today <- liftEffect nowDate
    let account' = normalizedAccount account
    nickname' <-
        ( validateNickname nickname
        <* sequence_ (accountChecks allContactKinds countries today account')
        )
        # AsyncVal.fromValidated
        # lmap (map (inj (Proxy :: _ "facts") >>> badRequest_))
    void $ pool # query queryString
        ( unwrap id : unwrap nickname'
        : toNullable account'.country : account'.languages
        : toNullable account'.birthday : toNullable account'.timezone
        :| writeImpl account'.contacts
        )
        # lmap \error -> case code error == unique_violation, constraint error of
            true, Just "player_nickname_key" -> nicknameTaken error
            true, Just "player_lower_nickname_key" -> nicknameTaken error
            _, _ -> Terror internal__ $ databaseErrorLines error
    pure noContent_
    where
    nicknameTaken error = Terror
        (badRequest_ $ inj (Proxy :: _ "nicknameTaken") {})
        [ "Player nickname is taken: " <> nickname, print error ]

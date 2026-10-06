module TeamTavern.Server.Infrastructure.SteamOpenId (SteamOpenIdUrl(..), steamEndpoint, verifySteamReturn) where

import Prelude

import Async (Async, attempt, left)
import Data.Array (all, elem)
import Data.Array.NonEmpty as Nea
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.String (Pattern(..), split, stripPrefix, trim)
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Variant (Variant, inj)
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Jarilo (BadRequestRow, InternalRow_, badRequest_, internal__)
import JavaScript.Error (message, name)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:|))
import JavaScript.Web.DOM.AbortSignal (timeout)
import JavaScript.Web.Fetch.Async (fetch, status, text)
import JavaScript.Web.URL.URLSearchParams as URLSearchParams
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstMaybe)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

-- | Where Steam's OpenID provider is asked to check what it answered,
-- | `https://steamcommunity.com/openid/login` outside the test stack.
newtype SteamOpenIdUrl = SteamOpenIdUrl String

-- | The provider every answer of Steam's names, wherever the check is sent.
steamEndpoint :: String
steamEndpoint = "https://steamcommunity.com/openid/login"

-- An individual account's SteamID64, the only kind a player signs in with.
claimedIdRegex :: Regex
claimedIdRegex = unsafeRegex "^https://steamcommunity\\.com/openid/id/(7656119\\d{10})$" noFlags

-- The UTC time an OpenID nonce starts with.
nonceTimeRegex :: Regex
nonceTimeRegex = unsafeRegex "^(\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z)" noFlags

-- What Steam's signature has to cover for the answer to say anything.
signedFields :: Array String
signedFields = [ "op_endpoint", "claimed_id", "identity", "return_to", "response_nonce" ]

-- An answer counts within five minutes either side of its nonce's time, and
-- its nonce is kept for a quarter hour, past any time it could count again.
acceptNonceQuery :: Query
acceptNonceQuery = Query """
    with stale as (
        delete from steam_nonce where accepted < now() - interval '15 minutes'
    )
    insert into steam_nonce (nonce)
    select $1
    where $2::timestamptz between now() - interval '5 minutes' and now() + interval '5 minutes'
    on conflict do nothing
    returning nonce
    """

type VerifyError other errors =
    TerrorVar (InternalRow_ + BadRequestRow (Variant (steamRefused :: {} | other)) + errors)

-- An answer that doesn't hold is the browser's to explain, not the server's.
refuse :: ∀ other errors a. String -> Async (VerifyError other errors) a
refuse line = left $ Terror (badRequest_ $ inj (Proxy :: _ "steamRefused") {}) [ "Refused Steam's answer: " <> line ]

-- | The SteamID64 Steam vouches for, from the `openid.` parameters it sent the
-- | browser back to the sign-in page with. The answer has to be for this
-- | origin's sign-in page, so one another site got for the same Steam account
-- | signs nobody in here. Steam checks its own signature, and the answer counts
-- | once and only around the time Steam gave it, so a reload, or a link
-- | carrying the parameters, is refused as `steamRefused`. Steam failing to
-- | answer is `internal`.
verifySteamReturn :: ∀ querier other errors. Querier querier =>
    SteamOpenIdUrl -> String -> querier -> Object String -> Async (VerifyError other errors) String
verifySteamReturn (SteamOpenIdUrl url) origin querier assertion = do
    let params = assertion # Object.filterKeys (stripPrefix (Pattern "openid.") >>> isJust)
        param key = Object.lookup ("openid." <> key) params
        signed = param "signed" # maybe [] (split (Pattern ","))
    when (param "mode" /= Just "id_res") $
        refuse $ "Its mode is " <> show (param "mode") <> "."
    when (param "op_endpoint" /= Just steamEndpoint) $
        refuse $ "It names the provider " <> show (param "op_endpoint") <> "."
    unless (param "return_to" >>= stripPrefix (Pattern $ origin <> "/signin?") # isJust) $
        refuse $ "It returns to " <> show (param "return_to") <> ", not to " <> origin <> "/signin."
    unless (signedFields # all (_ `elem` signed)) $
        refuse $ "It signs only " <> show signed <> "."
    steamId <- case param "claimed_id", param "identity" of
        Just claimedId, Just identity
            | claimedId == identity
            , Just [ _, Just steamId ] <- match claimedIdRegex claimedId <#> Nea.toArray -> pure steamId
        claimedId, _ -> refuse $ "It claims " <> show claimedId <> "."
    { nonce, nonceTime } <- case param "response_nonce" of
        Just nonce | Just [ _, Just nonceTime ] <- match nonceTimeRegex nonce <#> Nea.toArray -> pure { nonce, nonceTime }
        nonce -> refuse $ "Its nonce is " <> show nonce <> "."
    body <- liftEffect $ URLSearchParams.new (params # Object.insert "openid.mode" "check_authentication")
        >>= URLSearchParams.toString
    signal <- liftEffect $ timeout 5000
    response <- fetch url
        { method: "POST"
        , headers: Object.singleton "Content-Type" "application/x-www-form-urlencoded"
        , body
        , signal
        }
        # attempt >>= case _ of
            Left error -> left $ Terror internal__
                [ "Error asking Steam to check its answer: " <> name error <> " " <> message error ]
            Right response -> pure response
    content <- text response # lmap \error -> Terror internal__
        [ "Error reading Steam's check of its answer: " <> message error ]
    when (status response /= 200) $
        left $ Terror internal__ [ "Steam checked its answer with " <> show (status response) <> ": " <> content ]
    unless (elem "is_valid:true" (split (Pattern "\n") content <#> trim)) $
        refuse $ "Steam says it didn't sign it: " <> content
    accepted :: Maybe { nonce :: String } <- queryFirstMaybe querier acceptNonceQuery (nonce :| nonceTime)
    when (isNothing accepted) $
        refuse $ "Its nonce " <> nonce <> " counted already, or is not of now."
    pure steamId

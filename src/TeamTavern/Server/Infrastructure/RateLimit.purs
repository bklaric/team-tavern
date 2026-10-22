module TeamTavern.Server.Infrastructure.RateLimit
    ( Admission(..)
    , ClientLimits(..)
    , Limit
    , Limiter
    , admit
    , clientAddress
    , createClientLimiter
    , createLimiter
    , ensureAdmitted
    ) where

import Prelude

import Async (Async, fromEffect, left)
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Int (ceil, toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.MultiMap as MultiMap
import Data.Newtype (unwrap)
import Data.String (Pattern(..), contains, joinWith, split, toLower, trim)
import Data.String.CodeUnits (drop, lastIndexOf)
import Effect (Effect)
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Jarilo (TooManyRequestsRow, tooManyRequests)
import JavaScript.Node.Timers (setInterval)
import TeamTavern.Routes.Shared.Wait (Wait)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.Log (logStamped)
import Type.Row (type (+))

-- | At most `count` within any `seconds`.
type Limit = { count :: Int, seconds :: Int }

-- The times, in milliseconds, of what each key was let through for within
-- the limit's span, oldest first, and how many were refused since the last
-- sweep.
type State = { admitted :: Map String (Array Number), refused :: Int }

-- | A limit kept in memory, so a restart starts it afresh.
data Limiter = Limiter Limit (Ref State) | Unlimited

-- | Whether the limits kept per client address apply. They don't on the local
-- | stack, where every spec comes from the one address.
newtype ClientLimits = ClientLimits Boolean

-- | Whether a limiter let one through, or refused it and how many seconds
-- | until it would let one more through.
data Admission = Admitted | Refused Int

-- | A limiter named `name` in the line that says, once a minute, how many it
-- | refused. Refusals are answers the client handles, so they are otherwise
-- | not logged.
createLimiter :: String -> Limit -> Effect Limiter
createLimiter name limit = do
    ref <- Ref.new { admitted: Map.empty, refused: 0 }
    void $ setInterval 60000 do
        time <- nowMilliseconds
        refused <- ref # Ref.modify' \state ->
            { state: { admitted: Map.mapMaybe (keep limit time) state.admitted, refused: 0 }
            , value: state.refused
            }
        when (refused > 0) $ logStamped $
            "Rate limited | " <> name <> " | " <> show refused <> " refused in the past minute"
    pure $ Limiter limit ref

-- | A limiter keyed by the client's address, which lets everything through
-- | where client limits are off.
createClientLimiter :: ClientLimits -> String -> Limit -> Effect Limiter
createClientLimiter (ClientLimits on) name limit =
    if on then createLimiter name limit else pure Unlimited

nowMilliseconds :: Effect Number
nowMilliseconds = now <#> unInstant <#> unwrap

-- The times still within the limit's span, or none when no time is left.
keep :: Limit -> Number -> Array Number -> Maybe (Array Number)
keep { seconds } time times = let
    kept = times # Array.filter (_ > time - toNumber seconds * 1000.0)
    in
    if Array.null kept then Nothing else Just kept

-- | Lets one through for `key` and counts it, or answers 429 with how long to
-- | wait, in `Retry-After` and in the body.
ensureAdmitted :: ∀ errors.
    Limiter -> String -> String -> Async (TerrorVar (TooManyRequestsRow Wait + errors)) Unit
ensureAdmitted limiter key line = fromEffect (admit limiter key) >>= case _ of
    Admitted -> pure unit
    Refused seconds -> left $ Terror
        (tooManyRequests (MultiMap.singleton' "Retry-After" $ show seconds) { seconds })
        [ line ]

-- | Lets one through for `key` and counts it, or refuses it.
admit :: Limiter -> String -> Effect Admission
admit Unlimited _ = pure Admitted
admit (Limiter limit ref) key = do
    time <- nowMilliseconds
    state <- Ref.read ref
    let times = Map.lookup key state.admitted >>= keep limit time # fromMaybe []
        span = toNumber limit.seconds * 1000.0
    if Array.length times < limit.count
    then do
        Ref.write state { admitted = Map.insert key (Array.snoc times time) state.admitted } ref
        pure Admitted
    else do
        Ref.write state { refused = state.refused + 1 } ref
        pure $ Refused $ max 1 $ ceil $
            (Array.head times # maybe span \oldest -> oldest + span - time) / 1000.0

-- | The address of the client that sent the request, which Caddy puts in
-- | `X-Forwarded-For` in place of any the client sent. An IPv6 address is cut
-- | to its /64, since whoever holds one address holds the whole /64.
clientAddress :: Map String String -> String
clientAddress headers =
    Map.lookup "x-forwarded-for" headers
    <#> split (Pattern ",") >>= Array.last
    <#> trim >>> toLower
    # maybe "unknown" prefix64

prefix64 :: String -> String
prefix64 address
    -- IPv4, or IPv4 written as IPv6 (::ffff:192.0.2.1).
    | contains (Pattern ".") address =
        lastIndexOf (Pattern ":") address # maybe address \index -> drop (index + 1) address
    | otherwise = let
        groups part = if part == "" then [] else split (Pattern ":") part
        expanded = case split (Pattern "::") address of
            [ head, tail ] -> let
                head' = groups head
                tail' = groups tail
                in
                head' <> Array.replicate (8 - Array.length head' - Array.length tail') "0" <> tail'
            _ -> groups address
        in
        Array.take 4 expanded # joinWith ":" # (_ <> "::/64")

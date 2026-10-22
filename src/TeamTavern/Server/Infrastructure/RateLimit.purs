module TeamTavern.Server.Infrastructure.RateLimit
    ( Admission(..)
    , ClientLimits(..)
    , Limit
    , Limiter
    , admit
    , admitAll
    , clear
    , clientAddress
    , countOne
    , createClientLimiter
    , createLimiter
    , ensureAdmission
    , ensureAdmitted
    , ensureRoom
    , release
    ) where

import Prelude

import Async (Async, fromEffect, left)
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Foldable (for_, maximum)
import Data.Int (ceil, toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.MultiMap as MultiMap
import Data.Newtype (unwrap)
import Data.String (Pattern(..), contains, joinWith, split, toLower, trim)
import Data.String.CodeUnits (drop, lastIndexOf)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
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
ensureAdmitted limiter key = ensureAdmission (admit limiter key)

-- | As `ensureAdmitted`, for an admission made some other way, such as by
-- | `admitAll`.
ensureAdmission :: ∀ errors.
    Effect Admission -> String -> Async (TerrorVar (TooManyRequestsRow Wait + errors)) Unit
ensureAdmission admission line = fromEffect admission >>= case _ of
    Admitted -> pure unit
    Refused seconds -> left $ Terror
        (tooManyRequests (MultiMap.singleton' "Retry-After" $ show seconds) { seconds })
        [ line ]

-- | Answers 429, as `ensureAdmitted` does, when `key` has had all it gets, but
-- | counts nothing, for a request that counts only once it has done what is
-- | limited, with `countOne`.
ensureRoom :: ∀ errors.
    Limiter -> String -> String -> Async (TerrorVar (TooManyRequestsRow Wait + errors)) Unit
ensureRoom limiter key = ensureAdmission do
    time <- nowMilliseconds
    waitFor limiter key time >>= case _ of
        Nothing -> pure Admitted
        Just seconds -> countRefusal limiter $> Refused seconds

-- | Counts one for `key`, whether or not it had room.
countOne :: Limiter -> String -> Effect Unit
countOne limiter key = nowMilliseconds >>= count limiter key

-- | Takes back the latest one counted for `key`, for a request counted before
-- | it knew whether it would count, so that requests at once can't all find
-- | room before any is counted.
release :: Limiter -> String -> Effect Unit
release Unlimited _ = pure unit
release (Limiter _ ref) key = ref # Ref.modify_ \state ->
    state { admitted = Map.update (Array.init >=> \times -> if Array.null times then Nothing else Just times) key state.admitted }

-- | Forgets everything counted for `key`.
clear :: Limiter -> String -> Effect Unit
clear Unlimited _ = pure unit
clear (Limiter _ ref) key = ref # Ref.modify_ \state -> state { admitted = Map.delete key state.admitted }

-- | Lets one through for `key` and counts it, or refuses it.
admit :: Limiter -> String -> Effect Admission
admit limiter = admitAll [ limiter ]

-- | Lets one through for `key` when every limiter would, and counts it in
-- | each, or counts it in none and answers the longest wait. Each limiter that
-- | refused counts the refusal.
admitAll :: Array Limiter -> String -> Effect Admission
admitAll limiters key = do
    time <- nowMilliseconds
    waits <- for limiters \limiter -> waitFor limiter key time
    case Array.catMaybes waits # maximum of
        Nothing -> do
            for_ limiters \limiter -> count limiter key time
            pure Admitted
        Just seconds -> do
            for_ (Array.zip limiters waits) \(Tuple limiter wait) ->
                when (isJust wait) $ countRefusal limiter
            pure $ Refused seconds

-- The times `key` was let through for within the limit's span.
timesOf :: Limit -> State -> String -> Number -> Array Number
timesOf limit state key time = Map.lookup key state.admitted >>= keep limit time # fromMaybe []

-- The seconds until `key` would be let through, if it wouldn't be now.
waitFor :: Limiter -> String -> Number -> Effect (Maybe Int)
waitFor Unlimited _ _ = pure Nothing
waitFor (Limiter limit ref) key time = do
    state <- Ref.read ref
    let times = timesOf limit state key time
        span = toNumber limit.seconds * 1000.0
    pure if Array.length times < limit.count
        then Nothing
        else Just $ max 1 $ ceil $
            (Array.head times # maybe span \oldest -> oldest + span - time) / 1000.0

count :: Limiter -> String -> Number -> Effect Unit
count Unlimited _ _ = pure unit
count (Limiter limit ref) key time = ref # Ref.modify_ \state ->
    state { admitted = Map.insert key (Array.snoc (timesOf limit state key time) time) state.admitted }

countRefusal :: Limiter -> Effect Unit
countRefusal Unlimited = pure unit
countRefusal (Limiter _ ref) = ref # Ref.modify_ \state -> state { refused = state.refused + 1 }

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

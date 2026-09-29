module TeamTavern.Client.Shared.Fetch where

import Prelude

import Async (Async, attempt, fromEither)
import Async.Promisey (promiseToAsync)
import Data.Array (elem)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, onMatch)
import Data.Variant.Internal (VariantRep(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Jarilo as Jarilo
import Jarilo.Fetch (class Fetch)
import Jarilo.Fetch.Error (FetchError(..))
import Jarilo.Fetch.Method (class FetchMethod, fetchMethod)
import Jarilo.Fetch.Url (class FetchPath, fetchUrl)
import Jarilo.Types (FullRequest, FullRoute, NoQuery, Route)
import Literals (StringLit, stringLit)
import TeamTavern.Client.Shared.ClientError (reportClientError)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | A route, with the failures its caller handles as a matter of course, by
-- | the labels of their responses. The server hears of any other failure the
-- | route answers with. A bare proxy expects none.
data Expecting (route :: Route) = Expecting (Array String)

expecting :: ∀ route. Array String -> Proxy route -> Expecting route
expecting labels _ = Expecting labels

class Target target (route :: Route) | target -> route where
    expected :: target -> Array String

instance Target (Proxy route) route where
    expected _ = []

instance Target (Expecting route) route where
    expected (Expecting labels) = labels

routeOf :: ∀ target route. Target target route => target -> Proxy route
routeOf _ = Proxy

-- The method and path of a request, without the query string, which may
-- carry what no log should.
class RequestLine (route :: Route) pathParams | route -> pathParams where
    requestLine :: Proxy route -> Record pathParams -> String

instance (FetchMethod method, FetchPath path pathParams) =>
    RequestLine (FullRoute (FullRequest method path query body) response) pathParams where
    requestLine _ pathParams =
        fetchMethod (Proxy :: _ method) <> " /api"
        <> either (const "") identity (fetchUrl (Proxy :: _ path) (Proxy :: _ NoQuery) pathParams {})

-- The labels of the failures a page can cause. Internal is the server's own,
-- which it logs itself.
failures :: Array String
failures = [ "badRequest", "notAuthorized", "forbidden", "notFound" ]

-- A variant carries its label at runtime, which Data.Variant lets out only
-- to a match naming it.
label :: ∀ responses. Variant responses -> String
label variant = case (unsafeCoerce variant :: VariantRep Unit) of
    VariantRep { type: type_ } -> type_

-- The network going away and a fetch the page aborted are nobody's bug.
report :: ∀ responses. String -> Array String -> Either FetchError (Variant responses) -> Effect Unit
report request expected' = case _ of
    Left (NetworkError _) -> pure unit
    Left Aborted -> pure unit
    Left error -> reportClientError (Just request) (show error)
    Right response -> do
        let label' = label response
        when (elem label' failures && not (elem label' expected')) $
            reportClientError (Just request) label'

fetch
    :: ∀ target route pathParams queryParams realBody responses
    .  Target target route
    => Fetch route pathParams queryParams realBody responses
    => RequestLine route pathParams
    => target
    -> Record pathParams
    -> Record queryParams
    -> realBody
    -> Async FetchError (Variant responses)
fetch target path query body = do
    let route = routeOf target
    result <- Jarilo.fetch route path query body "/api"
        { credentials: (stringLit :: StringLit "include") }
        # promiseToAsync
        # attempt
    liftEffect $ report (requestLine route path) (expected target) result
    fromEither result

fetchPath
    :: ∀ target route pathParams responses
    .  Target target route
    => Fetch route pathParams () Unit responses
    => RequestLine route pathParams
    => target
    -> Record pathParams
    -> Async FetchError (Variant responses)
fetchPath proxy path = fetch proxy path {} unit

fetchQuery
    :: ∀ target route queryParams responses
    .  Target target route
    => Fetch route () queryParams Unit responses
    => RequestLine route ()
    => target
    -> Record queryParams
    -> Async FetchError (Variant responses)
fetchQuery proxy query = fetch proxy {} query unit

fetchBody
    :: ∀ target route realBody responses
    .  Target target route
    => Fetch route () () realBody responses
    => RequestLine route ()
    => target
    -> realBody
    -> Async FetchError (Variant responses)
fetchBody proxy body = fetch proxy {} {} body

fetchPathQuery
    :: ∀ target route pathParams queryParams responses
    .  Target target route
    => Fetch route pathParams queryParams Unit responses
    => RequestLine route pathParams
    => target
    -> Record pathParams
    -> Record queryParams
    -> Async FetchError (Variant responses)
fetchPathQuery proxy path query = fetch proxy path query unit

fetchPathNoContent
    :: ∀ left target route pathParams noContent responses
    .  Target target route
    => Fetch route pathParams () Unit (noContent :: noContent | responses)
    => RequestLine route pathParams
    => target
    -> Record pathParams
    -> Async left (Maybe Unit)
fetchPathNoContent proxy path = fetchPath proxy path # attempt <#>
    case _ of
    Left _ -> Nothing
    Right response ->
        onMatch { noContent: const $ Just unit } (const Nothing) response

fetchPathBody
    :: ∀ target route pathParams realBody responses
    .  Target target route
    => Fetch route pathParams () realBody responses
    => RequestLine route pathParams
    => target
    -> Record pathParams
    -> realBody
    -> Async FetchError (Variant responses)
fetchPathBody proxy path body = fetch proxy path {} body

fetchSimple
    :: ∀ target route responses
    .  Target target route
    => Fetch route () () Unit responses
    => RequestLine route ()
    => target
    -> Async FetchError (Variant responses)
fetchSimple proxy = fetch proxy {} {} unit

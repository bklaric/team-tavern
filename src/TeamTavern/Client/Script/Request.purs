module TeamTavern.Client.Script.Request where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.String as String
import Simple.JSON (class ReadForeign, class WriteForeign)
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync

nothingIfEmpty :: String -> Maybe String
nothingIfEmpty string = if String.null string then Nothing else Just string

justIfInt :: String -> Maybe Int
justIfInt = Int.fromString

get :: forall left ok. ReadForeign ok => String -> Async left (Maybe ok)
get url = Async.unify do
    response <- Fetch.fetch_ url # lmap (const Nothing)
    case FetchRes.status response of
        200 -> FetchRes.text response >>= JsonAsync.readJSON # lmap (const Nothing)
        _ -> Async.left Nothing

withBody :: forall body ok bad left. WriteForeign body => ReadForeign ok => ReadForeign bad =>
    Method -> String -> body -> Async left (Maybe (Either bad ok))
withBody method url body = Async.unify do
    response
        <- Fetch.fetch url
        (  Fetch.method := method
        <> Fetch.credentials := Fetch.Include
        <> Fetch.body := Json.writeJSON body
        )
        # lmap (const Nothing)
    result <-
        case FetchRes.status response of
        200 -> FetchRes.text response >>= JsonAsync.readJSON # bimap (const Nothing) Right
        400 -> FetchRes.text response >>= JsonAsync.readJSON # bimap (const Nothing) Left
        _ -> Async.left Nothing
    pure $ Just result

post :: forall body ok bad left. WriteForeign body => ReadForeign ok => ReadForeign bad =>
    String -> body -> Async left (Maybe (Either bad ok))
post = withBody POST

put :: forall body ok bad left. WriteForeign body => ReadForeign ok => ReadForeign bad =>
    String -> body -> Async left (Maybe (Either bad ok))
put = withBody PUT

withBodyNoContent :: forall body bad left. WriteForeign body => ReadForeign bad =>
    Method -> String -> body -> Async left (Maybe (Either bad Unit))
withBodyNoContent method url body = Async.unify do
    response
        <- Fetch.fetch url
        (  Fetch.method := method
        <> Fetch.credentials := Fetch.Include
        <> Fetch.body := Json.writeJSON body
        )
        # lmap (const Nothing)
    result <-
        case FetchRes.status response of
        204 -> pure $ Right unit
        400 -> FetchRes.text response >>= JsonAsync.readJSON # bimap (const Nothing) Left
        _ -> Async.left Nothing
    pure $ Just result

postNoContent :: forall body bad left. WriteForeign body => ReadForeign bad =>
    String -> body -> Async left (Maybe (Either bad Unit))
postNoContent = withBodyNoContent POST

putNoContent :: forall body bad left. WriteForeign body => ReadForeign bad =>
    String -> body -> Async left (Maybe (Either bad Unit))
putNoContent = withBodyNoContent PUT

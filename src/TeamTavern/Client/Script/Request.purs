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

post :: forall body ok bad left. WriteForeign body => ReadForeign ok => ReadForeign bad =>
    body -> Async left (Maybe (Either bad ok))
post body = Async.unify do
    response
        <- Fetch.fetch ("/api/teams")
        (  Fetch.method := POST
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

module Main where

import Prelude

import Data.Either (Either, either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Node.Buffer (concat_, toString___)
import Node.Http.Server (Request, Response, create)
import Node.Http.ServerRequest (url)
import Node.Server (ListenOptions(..), listen_)
import Node.Stream.Readable (class Readable)
import Node.Stream.Readable.Events (collectDataEvents)
import Node.Stream.Writable (endString__, writeString__)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Combinators (optionMaybe)
import URI (PathAbsolute)
import URI.Common (wrapParser)
import URI.Extra.QueryPairs (Key, QueryPairs, Value)
import URI.Extra.QueryPairs as QueryPairs
import URI.Path.Absolute (parse) as PathAbsolute
import URI.Query as Query
import Unsafe.Coerce (unsafeCoerce)

listenOptions :: ListenOptions
listenOptions = TcpListenOptions
    { port: Just 8080
    , host: Nothing
    , backlog: Nothing
    , exclusive: Nothing
    }

-- For server library (because unsafePartial)
readBuffersAsUtf8 :: forall readable. Readable readable =>
    (String -> Effect Unit) -> readable -> Effect readable
readBuffersAsUtf8 callback readable = unsafePartial do
    readable # collectDataEvents
        (map unsafeCoerce
        >>> concat_
        >=> toString___
        >=> callback)

data Url = Url PathAbsolute (Maybe (QueryPairs Key Value))

derive instance genericQueryPairs ∷ Generic Url _
instance name :: Show Url where
    show = genericShow

parseQueryString :: Parser String (Maybe (QueryPairs Key Value))
parseQueryString =
    optionMaybe $ wrapParser (QueryPairs.parse pure pure) Query.parser

parseUrl :: String -> Either ParseError Url
parseUrl requestUrl = runParser requestUrl $
    Url <$> PathAbsolute.parse <*> parseQueryString

requestHandler :: Request -> Response -> Effect Unit
requestHandler request response = do
    request # readBuffersAsUtf8 (\body -> do
        writeString__ body response # void
        writeString__ "\n" response # void
        request # url # parseUrl # either show show # flip endString__ response # void) # void

main :: Effect Unit
main = do
    server <- create requestHandler
    server # listen_ listenOptions

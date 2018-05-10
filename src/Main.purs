module Main where

import Prelude

import Data.Either (Either, either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..), fromString)
import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (NonEmptyString, toString)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Global.Unsafe (unsafeStringify)
import Node.Buffer (concat_, toString___)
import Node.Http.Server (Request, Response, create)
import Node.Http.ServerRequest (method, url)
import Node.Server (ListenOptions(..), listen_)
import Node.Stream.Readable (class Readable)
import Node.Stream.Readable.Events (collectDataEvents)
import Node.Stream.Writable (end__, writeString__)
import Routing.Junction (type (:<|>), type (:=), JunctionProxy(..), junctionRouter')
import Routing.Method (Get, Post)
import Routing.Path (type (:>), End)
import Routing.Query (NoQuery)
import Routing.Route (Route)
import Routing.Segment (Capture, Literal)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Combinators (optionMaybe)
import URI (PathAbsolute(..))
import URI.Common (wrapParser)
import URI.Extra.QueryPairs (Key, QueryPairs(..), Value)
import URI.Extra.QueryPairs as QueryPairs
import URI.Path.Absolute as PathAbsolute
import URI.Path.Segment (PathSegment, unsafeSegmentFromString, unsafeSegmentNZToString)
import URI.Query as Query
import Unsafe.Coerce (unsafeCoerce)

listenOptions :: ListenOptions
listenOptions = TcpListenOptions
    { port: Just 8080
    , host: Nothing
    , backlog: Nothing
    , exclusive: Nothing
    }

-- For server library (because unsafeCoerce).
readBuffersAsUtf8 :: forall readable. Readable readable =>
    (String -> Effect Unit) -> readable -> Effect readable
readBuffersAsUtf8 callback readable = do
    readable # collectDataEvents
        (map unsafeCoerce
        >>> concat_
        >=> toString___
        >=> callback)

data Url = Url PathAbsolute (Maybe (QueryPairs Key Value))

derive instance genericQueryPairs ∷ Generic Url _
instance name :: Show Url where show = genericShow

parseQueryString :: Parser String (Maybe (QueryPairs Key Value))
parseQueryString =
    optionMaybe $ wrapParser (QueryPairs.parse pure pure) Query.parser

parseUrl :: String -> Either ParseError Url
parseUrl requestUrl = runParser requestUrl $
    Url <$> PathAbsolute.parse <*> parseQueryString

pathSegments :: Url -> List PathSegment
pathSegments url =
    case url of
    Url (PathAbsolute (Just (Tuple nonEmptySegment otherSegments))) _ ->
        nonEmptySegment
        # unsafeSegmentNZToString
        # toString
        # unsafeSegmentFromString
        # (_ : (otherSegments # fromFoldable))
    Url (PathAbsolute Nothing) _ -> Nil

queryPairs :: Url -> QueryPairs Key Value
queryPairs url =
    case url of
    Url _ (Just queryPairs') -> queryPairs'
    Url _ Nothing -> QueryPairs []

type ViewPlavers = Route Get (Literal "players" :> End) NoQuery

type ViewPlayer = Route Get (Literal "players" :> Capture "nickname" NonEmptyString :> End) NoQuery

type RegisterPlayer = Route Post (Literal "players" :> End) NoQuery

type TeamTavernRoutes
    =    "viewPlayers"    := ViewPlavers
    :<|> "viewPlayer"     := ViewPlayer
    :<|> "registerPlayer" := RegisterPlayer

requestHandler :: Request -> Response -> Effect Unit
requestHandler request response = do
    request # readBuffersAsUtf8 (\body -> do
        writeString__ body response # void
        writeString__ "\n" response # void
        let requestMethod = method request # fromString
        let requestUrl = url request # parseUrl
        let routedUrl = junctionRouter'
                            (JunctionProxy :: JunctionProxy TeamTavernRoutes)
                            (either id (const GET) requestMethod)
                            (either (const Nil) pathSegments requestUrl)
                            (either (const $ QueryPairs []) queryPairs requestUrl)
        writeString__ (either show show requestMethod) response # void
        writeString__ "\n" response # void
        writeString__ (either show show requestUrl) response # void
        writeString__ "\n" response # void
        writeString__ (unsafeStringify routedUrl) response # void
        end__ response # void) # void

main :: Effect Unit
main = do
    server <- create requestHandler
    server # listen_ listenOptions

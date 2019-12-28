module TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url, UrlError, create, toString) where

import Prelude

import Data.Array ((!!))
import Data.Either (Either(..), isRight)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.String (toLower, trim)
import Data.String.Utils (endsWith, startsWith)
import Data.Variant (Variant)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser (runParser) as Parser
import Text.Parsing.Parser.String (string)
import URI (Path(..), RegName, Scheme)
import URI.Common (URIPartParseError(..), wrapParser)
import URI.Host.RegName as RegName
import URI.Path as Path
import URI.Path.Segment as Segment
import URI.Scheme as Scheme
import Wrapped as Wrapped
import Wrapped.String (Invalid, TooLong, invalid, tooLong)

type Domain = String

newtype Url = Url String

derive instance genericUrl :: Generic Url _

instance showUrl :: Show Url where
    show = genericShow

type UrlError = Variant (invalid :: Invalid, tooLong :: TooLong)

type ParsedUrl =
    { scheme :: Scheme
    , host :: RegName
    , path :: Path
    }

prependScheme :: String -> String
prependScheme url =
    if startsWith "http://" url || startsWith "https://" url
    then url
    else "https://" <> url

parseScheme :: Parser String Scheme
parseScheme = flip wrapParser Scheme.parser
    \scheme ->
        case Scheme.print scheme of
        "https:" -> Right scheme
        "http:" -> Right scheme
        scheme' -> Left $ URIPartParseError $ "Unsupported scheme: " <> scheme'

parseHost :: Domain -> Parser String RegName
parseHost domain = flip wrapParser RegName.parser
    \host ->
        if RegName.print host # toLower # endsWith domain
        then Right host
        else Left $ URIPartParseError $ "Wrong domain: " <> RegName.print host

parsePath :: Parser String Path
parsePath = flip wrapParser Path.parser
    \path @ (Path segments) ->
        case segments !! 0 of
        Just segment | Segment.printSegment segment /= "" -> Right path
        _ -> Left $ URIPartParseError $ "Invalid path: " <> Path.print path

parser :: Domain -> Parser String ParsedUrl
parser domain = do
  scheme <- parseScheme
  _ <- string "//"
  host <- parseHost domain
  path <- parsePath
  pure { scheme, host, path }

create :: Domain -> String -> Either (NonEmptyList UrlError) Url
create domain url =
    Wrapped.create
        (trim >>> prependScheme)
        [invalid ((flip Parser.runParser $ parser domain) >>> isRight), tooLong 200]
        Url url

toString :: Url -> String
toString (Url url) = url

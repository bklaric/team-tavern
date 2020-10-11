module TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url, UrlError, UrlErrors, validateUrl, validateUrlV, validateUrl_, validateUrlV_) where

import Prelude

import Data.Array ((!!))
import Data.Either (Either(..), isRight)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.String (toLower, trim)
import Data.String.Utils (endsWith, startsWith)
import Data.Validated (Validated)
import Data.Validated as Validated
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

derive newtype instance showUrl :: Show Url

type UrlError = Variant (invalid :: Invalid, tooLong :: TooLong)

type UrlErrors = NonEmptyList UrlError

type ParsedUrl =
    { scheme :: Scheme
    , host :: RegName
    , path :: Path
    }

prependScheme :: String -> String
prependScheme url =
    if startsWith "http://" (toLower url) || startsWith "https://" (toLower url)
    then url
    else "https://" <> url

parseScheme :: Parser String Scheme
parseScheme = flip wrapParser Scheme.parser
    \scheme ->
        case toLower $ Scheme.print scheme of
        "https:" -> Right scheme
        "http:" -> Right scheme
        scheme' -> Left $ URIPartParseError $ "Unsupported scheme: " <> scheme'

parseHost :: Domain -> Parser String RegName
parseHost domain = flip wrapParser RegName.parser
    \host ->
        if RegName.print host # toLower # endsWith domain
        then Right host
        else Left $ URIPartParseError $ "Wrong domain: " <> RegName.print host

parseHost_ :: Parser String RegName
parseHost_ = wrapParser Right RegName.parser

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

parser_ :: Parser String ParsedUrl
parser_ = do
    scheme <- parseScheme
    _ <- string "//"
    host <- parseHost_
    path <- parsePath
    pure { scheme, host, path }

validateUrl :: Domain -> String -> Either (NonEmptyList UrlError) Url
validateUrl domain url =
    Wrapped.create
        (trim >>> prependScheme)
        [invalid ((flip Parser.runParser $ parser domain) >>> isRight), tooLong 200]
        Url url

validateUrlV :: String -> String -> Validated (NonEmptyList UrlError) Url
validateUrlV domain url = validateUrl domain url # Validated.fromEither

validateUrl_ :: String -> Either (NonEmptyList UrlError) Url
validateUrl_ url =
    Wrapped.create
        (trim >>> prependScheme)
        [invalid ((flip Parser.runParser parser_) >>> isRight), tooLong 200]
        Url url

validateUrlV_ :: String -> Validated (NonEmptyList UrlError) Url
validateUrlV_ url = validateUrl_ url # Validated.fromEither

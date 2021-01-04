module TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url, toString, UrlError, UrlErrors, validateUrl, validateUrlV, validateUrl_, validateUrlV_) where

import Prelude

import Data.Array (intercalate, (!!))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), maybe)
import Data.String (toLower, trim)
import Data.String as String
import Data.String.NonEmpty as NES
import Data.String.Utils (endsWith, startsWith)
import Data.Validated (Validated)
import Data.Validated as Validated
import Data.Variant (SProxy(..), Variant, inj)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser (runParser) as Parser
import Text.Parsing.Parser.Combinators (notFollowedBy, optionMaybe)
import Text.Parsing.Parser.String (anyChar, string)
import URI (Path(..), RegName, Scheme)
import URI.Common (URIPartParseError(..), wrapParser)
import URI.Host.RegName as Host
import URI.Host.RegName as RegName
import URI.Path as Path
import URI.Path.Segment (segmentToString)
import URI.Path.Segment as Segment
import URI.Scheme as Scheme
import Wrapped.String (Invalid, TooLong)

type Domain = String

newtype Url = Url String

derive newtype instance showUrl :: Show Url

toString :: Url -> String
toString (Url url) = url

type UrlError = Variant (invalid :: Invalid, tooLong :: TooLong)

type UrlErrors = NonEmptyList UrlError

type ParsedUrl =
    { scheme :: Scheme
    , host :: RegName
    , path :: Maybe Path
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
    \host -> let
        printedHost = RegName.print host # toLower
        in
        if printedHost # endsWith domain
        then Right host
        else Left $ URIPartParseError $ "Wrong domain: " <> RegName.print host

parseHost_ :: Parser String RegName
parseHost_ = RegName.parser

parsePath :: Parser String Path
parsePath = flip wrapParser Path.parser
    \path @ (Path segments) ->
        case segments !! 0 of
        Just segment | Segment.printSegment segment /= "" -> Right path
        _ -> Left $ URIPartParseError $ "Invalid path: " <> Path.print path

parsePath_ :: Parser String (Maybe Path)
parsePath_ = optionMaybe parsePath

parser :: Domain -> Parser String ParsedUrl
parser domain = do
  scheme <- parseScheme
  _ <- string "//"
  host <- parseHost domain
  path <- parsePath
  notFollowedBy anyChar
  pure { scheme, host, path: Just path }

parser_ :: Parser String ParsedUrl
parser_ = do
    scheme <- parseScheme
    _ <- string "//"
    host <- parseHost_
    path <- parsePath_
    notFollowedBy anyChar
    pure { scheme, host, path }

validateUrl :: Domain -> String -> Either (NonEmptyList UrlError) Url
validateUrl domain url =
    case Parser.runParser (url # trim # prependScheme) (parser domain)
        <#> (\{ scheme, host, path } -> (NES.toString $ Scheme.toString scheme) <> "://" <> (NES.toString $ Host.toString host) <> (maybe "" (\(Path segments) -> "/" <> intercalate "/" (segments <#> segmentToString) ) path))
        # lmap (const $ NonEmptyList.singleton $ inj (SProxy :: SProxy "invalid") { original: url }) of
    Left errors -> Left errors
    Right url' ->
        if String.length url' <= 200
        then Right $ Url url'
        else Left $ NonEmptyList.singleton $ inj (SProxy :: SProxy "tooLong") { actualLength: String.length url, maxLength: 200 }

validateUrlV :: String -> String -> Validated (NonEmptyList UrlError) Url
validateUrlV domain url = validateUrl domain url # Validated.fromEither

validateUrl_ :: String -> Either (NonEmptyList UrlError) Url
validateUrl_ url =
    case Parser.runParser (url # trim # prependScheme) (parser_)
        <#> (\{ scheme, host, path } -> (NES.toString $ Scheme.toString scheme) <> "://" <> (NES.toString $ Host.toString host) <> (maybe "" (\(Path segments) -> "/" <> intercalate "/" (segments <#> segmentToString) ) path))
        # lmap (const $ NonEmptyList.singleton $ inj (SProxy :: SProxy "invalid") { original: url }) of
    Left errors -> Left errors
    Right url' ->
        if String.length url' <= 200
        then Right $ Url url'
        else Left $ NonEmptyList.singleton $ inj (SProxy :: SProxy "tooLong") { actualLength: String.length url, maxLength: 200 }

validateUrlV_ :: String -> Validated (NonEmptyList UrlError) Url
validateUrlV_ url = validateUrl_ url # Validated.fromEither

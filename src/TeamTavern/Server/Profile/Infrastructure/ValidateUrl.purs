module TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url, UrlError, create, toString) where

import Prelude

import Data.Either (Either, isRight)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Types (NonEmptyList)
import Data.String (Pattern(..), contains, trim)
import Data.Variant (Variant)
import Text.Parsing.Parser (runParser)
import URI (Fragment, HierPath, Host, Path, Port, Query, UserInfo)
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair
import URI.URI (URIOptions, parser)
import Wrapped as Wrapped
import Wrapped.String (Invalid, TooLong, invalid, tooLong)

newtype Url = Url String

derive instance genericUrl :: Generic Url _

instance showUrl :: Show Url where
    show = genericShow

type UrlError = Variant (invalid :: Invalid, tooLong :: TooLong)

options ∷ Record (URIOptions
    UserInfo (HostPortPair Host Port) Path HierPath Query Fragment)
options =
  { parseUserInfo: pure
  , printUserInfo: identity
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print identity identity
  , parsePath: pure
  , printPath: identity
  , parseHierPath: pure
  , printHierPath: identity
  , parseQuery: pure
  , printQuery: identity
  , parseFragment: pure
  , printFragment: identity
  }

prependScheme :: String -> String
prependScheme url =
    if contains (Pattern "http://") url || contains (Pattern "https://") url
    then url
    else "http://" <> url

create :: String -> Either (NonEmptyList UrlError) Url
create url =
    Wrapped.create
        (trim >>> prependScheme)
        [invalid ((flip runParser $ parser options) >>> isRight), tooLong 200]
        Url url

toString :: Url -> String
toString (Url url) = url

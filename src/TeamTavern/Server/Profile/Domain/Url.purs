module TeamTavern.Server.Profile.Domain.Url (Url(..), UrlError, create) where

import Prelude

import Data.Either (Either, isRight)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.String (trim)
import Data.Variant (Variant)
import Text.Parsing.Parser (runParser)
import URI (Fragment, HierPath, Host, Path, Port, Query, RelPath, URIRef, UserInfo)
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair
import URI.URIRef (URIRefOptions, parser)
import Wrapped as Wrapped
import Wrapped.String (Invalid, TooLong, invalid, tooLong)

newtype Url = Url String

derive instance newtypeUrl :: Newtype Url _

derive instance genericUrl :: Generic Url _

instance showUrl :: Show Url where
    show = genericShow

type UrlError = Variant (invalid :: Invalid, tooLong :: TooLong)

type MyURI = URIRef
    UserInfo (HostPortPair Host Port) Path HierPath RelPath Query Fragment

options ∷ Record (URIRefOptions
    UserInfo (HostPortPair Host Port) Path HierPath RelPath Query Fragment)
options =
  { parseUserInfo: pure
  , printUserInfo: identity
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print identity identity
  , parsePath: pure
  , printPath: identity
  , parseHierPath: pure
  , printHierPath: identity
  , parseRelPath: pure
  , printRelPath: identity
  , parseQuery: pure
  , printQuery: identity
  , parseFragment: pure
  , printFragment: identity
  }

create :: String -> Either (NonEmptyList UrlError) Url
create url =
    Wrapped.create
        trim
        [invalid ((flip runParser $ parser options) >>> isRight), tooLong 200]
        Url url

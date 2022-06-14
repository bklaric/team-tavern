module Perun.Url (Url(..), parseUrl, pathSegments, queryPairs) where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (toString)
import Data.Tuple (Tuple(..))
import Parsing (ParseError, Parser, runParser)
import Parsing.Combinators (optionMaybe)
import URI (PathAbsolute(..))
import URI.Common (wrapParser)
import URI.Extra.QueryPairs (Key, QueryPairs(..), Value)
import URI.Extra.QueryPairs as QueryPairs
import URI.Path.Absolute as PathAbsolute
import URI.Path.Segment (PathSegment, unsafeSegmentFromString, unsafeSegmentNZToString)
import URI.Query as Query

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

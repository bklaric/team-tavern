module Jarilo.Router.Path where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Jarilo.Shared.Component (class Component, fromComponent)
import Jarilo.Types (Path, Literal, Capture, PathChain)
import Prim.Row (class Cons, class Lacks)
import Record.Builder (Builder, buildFromScratch, insert)
import Type.Proxy (Proxy(..))
import URI.Path.Segment (PathSegment, segmentToString, unsafeSegmentFromString)

data PathError
    = DoesntEndButShould
        { restOfPath :: List PathSegment }
    | EndsButShouldnt
        { expectedSegment :: String }
    | WrongLiteral
        { expectedLiteral :: PathSegment
        , actualLiteral :: PathSegment
        }
    | WrongCapture
        { segmentName :: String
        , errorMessage :: String
        , actualSegment :: PathSegment
        }

class PathRouter (path :: Path) (input :: Row Type) (output :: Row Type) | path -> input output where
    pathRouter'
        :: Proxy path
        -> List PathSegment
        -> Either
            PathError
            (Tuple
                (List PathSegment)
                (Builder (Record input) (Record output)))

instance IsSymbol literal => PathRouter (Literal literal) input input where
    pathRouter' _ Nil = Left $ EndsButShouldnt
        { expectedSegment: reflectSymbol (Proxy :: _ literal) }
    pathRouter' _ (actualLiteral : path) = do
        let expectedLiteral = reflectSymbol (Proxy :: _ literal) # unsafeSegmentFromString
        builder <- if expectedLiteral == actualLiteral
            then Right identity
            else Left $ WrongLiteral
                { expectedLiteral: expectedLiteral
                , actualLiteral: actualLiteral
                }
        Right $ Tuple path builder

instance
    ( IsSymbol name
    , Lacks name input
    , Cons name value input output
    , Component value
    ) =>
    PathRouter (Capture name value) input output where
    pathRouter' _ Nil = Left $ EndsButShouldnt
        { expectedSegment: reflectSymbol (Proxy :: _ name) }
    pathRouter' _ (segmentToCapture : path) =
        segmentToCapture # segmentToString # (fromComponent :: _ -> _ _ value) # bimap
            (\message -> WrongCapture
                { segmentName: reflectSymbol (Proxy :: _ name)
                , errorMessage: message
                , actualSegment: segmentToCapture
                })
            (Tuple path <<< insert (Proxy :: _ name))

instance
    ( PathRouter leftPath input midput
    , PathRouter rightPath midput output
    ) =>
    PathRouter (PathChain leftPath rightPath) input output where
    pathRouter' _ path = let
        leftProxy = (Proxy :: _ leftPath)
        rightProxy = (Proxy :: _ rightPath)
        in do
        Tuple leftPath leftBuilder <- pathRouter' leftProxy path
        Tuple rightPath rightBuilder <- pathRouter' rightProxy leftPath
        pure $ Tuple rightPath $ leftBuilder >>> rightBuilder

pathRouter :: ∀ path output. PathRouter path () output =>
    Proxy path -> List PathSegment -> Either PathError (Record output)
pathRouter proxy path = do
    Tuple restOfPath pathBuilder <- pathRouter' proxy path
    case restOfPath of
        Nil -> Right $ buildFromScratch pathBuilder
        _ -> Left $ DoesntEndButShould { restOfPath }

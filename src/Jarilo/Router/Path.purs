module Jarilo.Router.Path where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Jarilo.Path (Path, Literal, Capture, Sub)
import Jarilo.Shared.Component (class Component, fromComponent)
import Prim.Row (class Cons, class Lacks)
import Record.Builder (Builder, insert)
import Type.Proxy (Proxy(..))
import URI.Path.Segment (PathSegment, segmentToString, unsafeSegmentFromString)

type PathError = Variant
    ( notEnd ::
        { restOfPath :: List PathSegment }
    , segmentEnd ::
        { expectedSegment :: String }
    , literal ::
        { expectedLiteral :: PathSegment
        , actualLiteral :: PathSegment
        }
    , capture ::
        { segmentName :: String
        , errorMessage :: String
        , actualSegment :: PathSegment
        }
    )

class PathRouter (path :: Path) (input :: Row Type) (output :: Row Type) | path -> input output where
    pathRouter
        :: Proxy path
        -> List PathSegment
        -> Either
            PathError
            (Tuple
                (List PathSegment)
                (Builder (Record input) (Record output))
            )

instance IsSymbol literal => PathRouter (Literal literal) input input where
    pathRouter _ Nil =
        Left
        $ inj (Proxy :: _ "segmentEnd")
        { expectedSegment: reflectSymbol (Proxy :: _ literal) }
    pathRouter _ (actualLiteral : path) = do
        let expectedLiteral = reflectSymbol (Proxy :: _ literal) # unsafeSegmentFromString
        builder <- if expectedLiteral == actualLiteral
            then Right identity
            else Left $ inj (Proxy :: _ "literal")
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
    pathRouter _ Nil =
        Left
        $ inj (Proxy :: _ "segmentEnd")
        { expectedSegment: reflectSymbol (Proxy :: _ name) }
    pathRouter _ (segmentToCapture : path) =
        segmentToCapture # segmentToString # (fromComponent :: _ -> _ _ value) # bimap
            (\message -> inj (Proxy :: _ "capture")
                { segmentName: reflectSymbol (Proxy :: _ name)
                , errorMessage: message
                , actualSegment: segmentToCapture
                })
            (Tuple path <<< insert (Proxy :: _ name))

instance
    ( PathRouter leftPath input midput
    , PathRouter rightPath midput output
    ) =>
    PathRouter (Sub leftPath rightPath) input output where
    pathRouter _ path = let
        leftProxy = (Proxy :: _ leftPath)
        rightProxy = (Proxy :: _ rightPath)
        in do
        Tuple leftPath leftBuilder <- pathRouter leftProxy path
        Tuple rightPath rightBuilder <- pathRouter rightProxy leftPath
        pure $ Tuple rightPath $ leftBuilder >>> rightBuilder

pathRouter' :: forall input output path. PathRouter path input output =>
    Proxy path -> List PathSegment -> Either PathError (Builder (Record input) (Record output))
pathRouter' proxy path = do
    Tuple restOfPath pathBuilder <- pathRouter proxy path
    case restOfPath of
        Nil -> Right pathBuilder
        _ -> Left $ inj (Proxy :: _ "notEnd") { restOfPath }

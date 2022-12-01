module Jarilo.Router.Method where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (CustomMethod, Method(..)) as HM
import Data.Maybe (Maybe(..), maybe)
import Data.Variant (Variant, inj)
import Foreign (MultipleErrors)
import Jarilo.Method (Delete, Get, Head, Method, Options, Patch, Post, Put)
import Type.Proxy (Proxy(..))
import Yoga.JSON (class ReadForeign, readJSON)

type MethodError = Variant
    ( wrongMethod ::
        { expectedMethod :: Either HM.CustomMethod HM.Method
        , actualMethod :: Either HM.CustomMethod HM.Method
        }
    , bodyParse :: MultipleErrors
    )

checkMethod
    :: Either HM.CustomMethod HM.Method
    -> Either HM.CustomMethod HM.Method
    -> Maybe MethodError
checkMethod expectedMethod actualMethod =
    if expectedMethod == actualMethod
    then Nothing
    else Just $ inj (Proxy :: _ "wrongMethod") { expectedMethod, actualMethod }

checkMethod'
    :: Either HM.CustomMethod HM.Method
    -> Either HM.CustomMethod HM.Method
    -> Either MethodError Unit
checkMethod' expectedMethod actualMethod =
    checkMethod expectedMethod actualMethod # maybe (Right unit) Left

class MethodRouter (method :: Method) (body :: Type) | method -> body where
    methodRouter
        :: Proxy method
        -> Either HM.CustomMethod HM.Method
        -> String
        -> Either MethodError body

instance MethodRouter Options Unit where
    methodRouter _ actual _ = checkMethod' (Right HM.OPTIONS) actual

instance MethodRouter Head Unit where
    methodRouter _ actual _ = checkMethod' (Right HM.HEAD) actual

instance MethodRouter Get Unit where
    methodRouter _ actual _ = checkMethod' (Right HM.GET) actual

instance ReadForeign body => MethodRouter (Post body) body where
    methodRouter _ actual body =
        case checkMethod (Right HM.POST) actual of
        Just error -> Left error
        Nothing -> readJSON body # lmap (inj (Proxy :: _ "bodyParse"))

instance ReadForeign body => MethodRouter (Put body) body where
    methodRouter _ actual body =
        case checkMethod (Right HM.PUT) actual of
        Just error -> Left error
        Nothing -> readJSON body # lmap (inj (Proxy :: _ "bodyParse"))

instance ReadForeign body => MethodRouter (Patch body) body where
    methodRouter _ actual body =
        case checkMethod (Right HM.PATCH) actual of
        Just error -> Left error
        Nothing -> readJSON body # lmap (inj (Proxy :: _ "bodyParse"))

instance MethodRouter Delete Unit where
    methodRouter _ actual _ = checkMethod' (Right HM.DELETE) actual

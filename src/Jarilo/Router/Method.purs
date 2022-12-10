module Jarilo.Router.Method where

import Prelude

import Data.Either (Either(..))
import Data.HTTP.Method (CustomMethod, Method(..)) as HM
import Jarilo.Types (Delete, Get, Head, Method, Options, Patch, Post, Put)
import Type.Proxy (Proxy)

data MethodError = WrongMethod
    { expectedMethod :: Either HM.CustomMethod HM.Method
    , actualMethod :: Either HM.CustomMethod HM.Method
    }

methodRouter'
    :: Either HM.CustomMethod HM.Method
    -> Either HM.CustomMethod HM.Method
    -> Either MethodError Unit
methodRouter' expectedMethod actualMethod =
    if expectedMethod == actualMethod
    then Right unit
    else Left $ WrongMethod { expectedMethod, actualMethod }

class MethodRouter (method :: Method) where
    methodRouter
        :: Proxy method
        -> Either HM.CustomMethod HM.Method
        -> Either MethodError Unit

instance MethodRouter Options where
    methodRouter _ actual  = methodRouter' (Right HM.OPTIONS) actual

instance MethodRouter Head where
    methodRouter _ actual  = methodRouter' (Right HM.HEAD) actual

instance MethodRouter Get where
    methodRouter _ actual  = methodRouter' (Right HM.GET) actual

instance MethodRouter Post where
    methodRouter _ actual  = methodRouter' (Right HM.POST) actual

instance MethodRouter Put where
    methodRouter _ actual  = methodRouter' (Right HM.PUT) actual

instance MethodRouter Patch where
    methodRouter _ actual  = methodRouter' (Right HM.PATCH) actual

instance MethodRouter Delete where
    methodRouter _ actual  = methodRouter' (Right HM.DELETE) actual

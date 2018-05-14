module Perun.Request where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (CustomMethod, Method, fromString)
import Node.Http.Server.Request (method, url)
import Node.Http.Server.Request as Node
import Perun.Request.Body (Body, fromRequest)
import Perun.Url (Url, parseUrl)

type Request =
    { method :: Either CustomMethod Method
    , url :: Either String Url
    , body :: Body
    }

readRequest :: Node.Request -> Request
readRequest request =
    { method: request # method # fromString # case _ of
        Left m -> Right m
        Right cm -> Left cm
    , url: request # url # parseUrl # lmap (const $ url request)
    , body: fromRequest request
    }

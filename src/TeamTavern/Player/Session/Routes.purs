module TeamTavern.Player.Session.Routes where

import Data.String.NonEmpty (NonEmptyString)
import Jarilo.Method (Patch, Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)

type PrepareSession = Route
    Post
    (  Literal "players"
    :> Literal "by-nickname"
    :> Capture "nickname" NonEmptyString
    :> Literal "sessions"
    :> End)
    NoQuery

type StartSession = Route
    Patch
    (  Literal "players"
    :> Literal "by-nickname"
    :> Capture "nickname" NonEmptyString
    :> Literal "sessions"
    :> End)
    NoQuery

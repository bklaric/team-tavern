module TeamTavern.Profile.Routes where

import Jarilo.Junction (type (:=))
import Jarilo.Method (Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)

type CreateProfile = Route
    Post
    (  Literal "players"
    :> Capture "nickname" String
    :> Literal "profiles"
    :> Capture "handle" String
    :> End)
    NoQuery

type ProfileRoutes
    =    "createProfile" := CreateProfile

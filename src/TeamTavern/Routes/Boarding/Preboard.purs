module TeamTavern.Routes.Boarding.Preboard where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe)
import Data.Variant (Variant)
import Jarilo (type (!), type (==>), Literal, OkJson, PostJson_, BadRequestJson)
import TeamTavern.Routes.Boarding.Onboard (PlayerProfileRequestContent, PlayerRequestContent, TeamProfileRequestContent, TeamRequestContent)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContactsError, PlayerContacts)
import TeamTavern.Routes.Shared.TeamContacts (TeamContactsError, TeamContacts)
import Type.Function (type ($))

type Preboard =
    PostJson_ (Literal "preboarding") RequestContent
    ==> OkJson OkContent ! BadRequestJson BadContent

type RegisterRequestContent =
    { nickname :: String
    , password :: String
    }

type RequestContent =
    { ilk :: Int
    , player :: Maybe PlayerRequestContent
    , team :: Maybe TeamRequestContent
    , gameHandle :: String
    , playerProfile :: Maybe PlayerProfileRequestContent
    , teamProfile :: Maybe TeamProfileRequestContent
    , playerContacts :: Maybe PlayerContacts
    , teamContacts :: Maybe TeamContacts
    , registration :: RegisterRequestContent
    }

type OkContent = { teamHandle :: Maybe String }

type BadContent = NonEmptyArray $ Variant
    ( team :: NonEmptyArray $ Variant
        ( name :: {}
        , website :: {}
        )
    , playerProfile :: NonEmptyArray $ Variant
        ( url :: { key :: String }
        , about :: {}
        , ambitions :: {}
        )
    , teamProfile :: NonEmptyArray $ Variant
        ( platforms :: {}
        , about :: {}
        , ambitions :: {}
        )
    , playerContacts :: Array PlayerContactsError
    , teamContacts :: Array TeamContactsError
    , registration :: NonEmptyArray $ Variant
        ( nickname :: {}
        , password :: {}
        )
    , nicknameTaken :: {}
    )

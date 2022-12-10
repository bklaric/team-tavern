module TeamTavern.Client.Components.Team.TeamDetails (teamDetails) where

import Prelude

import Async (Async)
import Client.Components.Copyable as Copyable
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import TeamTavern.Client.Components.Detail (arrangedOrDetail, detail, urlDetail, weekdaysOnlineDetail, weekendsOnlineDetail)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Shared.Organization (OrganizationNW, websiteNW)

teamWebsiteDetail :: ∀ slots action. Maybe String -> Maybe (HH.HTML slots action)
teamWebsiteDetail website = urlDetail "fas fa-globe" "Website" website

teamAgeDetail :: ∀ slots action. Maybe Int -> Maybe Int -> Maybe (HH.HTML slots action)
teamAgeDetail Nothing Nothing = Nothing
teamAgeDetail (Just from) Nothing = Just $
    detail "fas fa-calendar-alt"
    [ HH.span [ HS.class_ "detail-labelless" ] [ HH.text "Are older than " ]
    , HH.span [ HS.class_ "detail-emphasize" ] [ HH.text $ show from ]
    ]
teamAgeDetail Nothing (Just to) = Just $
    detail "fas fa-calendar-alt"
    [ HH.span [ HS.class_ "detail-labelless" ] [ HH.text "Are younger than " ]
    , HH.span [ HS.class_ "detail-emphasize" ] [ HH.text $ show to ]
    ]
teamAgeDetail (Just from) (Just to) = Just $
    detail "fas fa-calendar-alt"
    [ HH.span [ HS.class_ "detail-labelless" ] [ HH.text "Are between " ]
    , HH.span [ HS.class_ "detail-emphasize" ] [ HH.text $ show from ]
    , HH.text " and "
    , HH.span [ HS.class_ "detail-emphasize" ] [ HH.text $ show to ]
    , HH.text " years old"
    ]

teamLocationsDetail :: ∀ slots actions. Array String -> Maybe (HH.HTML slots actions)
teamLocationsDetail locations = arrangedOrDetail "fas fa-globe-europe" "Live in" locations

teamLanguagesDetail :: ∀ slots actions. Array String -> Maybe (HH.HTML slots actions)
teamLanguagesDetail languages = arrangedOrDetail "fas fa-comments" "Speak" languages

teamMicrophoneDetail :: ∀ slots actions. Boolean -> Maybe (HH.HTML slots actions)
teamMicrophoneDetail false = Nothing
teamMicrophoneDetail true = Just $
    detail "fas fa-microphone"
    [ HH.span [ HS.class_ "detail-labelless detail-emphasize" ]
        [ HH.text "Have a microphone" ]
    , HH.text $ " and are willing to communicate"
    ]

teamDetails :: ∀ fields slots action left.
    { handle :: String
    , organization :: OrganizationNW
    , discordTag :: Maybe String
    , discordServer :: Maybe String
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , weekdayOnline :: Maybe
                        { from :: String
                        , to :: String
                        }
    , weekendOnline :: Maybe
                        { from :: String
                        , to :: String
                        }
    | fields
    }
    -> Array (HH.ComponentHTML action ( discordTag :: Copyable.Slot String | slots) (Async left))
teamDetails details =
    Array.catMaybes
    [ teamWebsiteDetail $ websiteNW details.organization
    , teamAgeDetail details.ageFrom details.ageTo
    , teamLocationsDetail details.locations
    , teamLanguagesDetail details.languages
    , teamMicrophoneDetail details.microphone
    , weekdaysOnlineDetail details.weekdayOnline
    , weekendsOnlineDetail details.weekendOnline
    ]

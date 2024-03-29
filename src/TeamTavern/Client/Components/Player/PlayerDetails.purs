module TeamTavern.Client.Components.Player.PlayerDetails (playerDetails) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import TeamTavern.Client.Components.Detail (arrangedAndDetail, detail, weekdaysOnlineDetail, weekendsOnlineDetail)
import TeamTavern.Client.Shared.Slot (Slot__String)
import TeamTavern.Client.Snippets.Class as HS

playerAgeDetail :: ∀ slots action. Maybe Int -> Maybe (HH.HTML slots action)
playerAgeDetail Nothing = Nothing
playerAgeDetail (Just age) = Just $
    detail "fas fa-calendar-alt"
    [ HH.span [ HS.class_ "detail-labelless" ] [ HH.text "Is " ]
    , HH.span [ HS.class_ "detail-emphasize" ] [ HH.text $ show age ]
    , HH.text " years old"
    ]

playerLocationDetail :: ∀ slots action. Maybe String -> Maybe (HH.HTML slots action)
playerLocationDetail Nothing = Nothing
playerLocationDetail (Just location) = Just $
    detail "fas fa-globe-europe"
    [ HH.span [ HS.class_ "detail-labelless" ] [ HH.text "Lives in " ]
    , HH.span [ HS.class_ "detail-emphasize" ] [ HH.text location ]
    ]

playerLanguagesDetail :: ∀ slots action. Array String -> Maybe (HH.HTML slots action)
playerLanguagesDetail languages = arrangedAndDetail "fas fa-comments" "Speaks" languages

playerMicrophoneDetail :: ∀ slots action. Boolean -> Maybe (HH.HTML slots action)
playerMicrophoneDetail false = Nothing
playerMicrophoneDetail true = Just $
    detail "fas fa-microphone"
    [ HH.span [ HS.class_ "detail-labelless detail-emphasize" ]
        [ HH.text "Has a microphone" ]
    , HH.text $ " and is willing to communicate"
    ]

playerDetails :: ∀ fields action slots left.
    { nickname :: String
    , age :: Maybe Int
    , languages :: Array String
    , location :: Maybe String
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
    -> Array (HH.ComponentHTML action (discordTag :: Slot__String | slots) (Async left))
playerDetails details =
    Array.catMaybes
    [ playerAgeDetail details.age
    , playerLocationDetail details.location
    , playerLanguagesDetail details.languages
    , playerMicrophoneDetail details.microphone
    , weekdaysOnlineDetail details.weekdayOnline
    , weekendsOnlineDetail details.weekendOnline
    ]

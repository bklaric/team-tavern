module TeamTavern.Client.Components.Player.PlayerDetails (playerDetails) where

import Prelude

import Async (Async)
import Client.Components.Copyable (copyable)
import Client.Components.Copyable as Copyable
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen.HTML as HH
import TeamTavern.Client.Components.Detail (arrangedAndDetail, detail, weekdaysOnlineDetail, weekendsOnlineDetail)
import TeamTavern.Client.Snippets.Class as HS

playerAgeDetail :: forall slots action. Maybe Int -> Maybe (HH.HTML slots action)
playerAgeDetail Nothing = Nothing
playerAgeDetail (Just age) = Just $
    detail "fas fa-calendar-alt"
    [ HH.span [ HS.class_ "detail-labelless" ] [ HH.text "Is " ]
    , HH.span [ HS.class_ "detail-emphasize" ] [ HH.text $ show age ]
    , HH.text " years old"
    ]

playerLocationDetail :: forall slots action. Maybe String -> Maybe (HH.HTML slots action)
playerLocationDetail Nothing = Nothing
playerLocationDetail (Just location) = Just $
    detail "fas fa-globe-europe"
    [ HH.span [ HS.class_ "detail-labelless" ] [ HH.text "Lives in " ]
    , HH.span [ HS.class_ "detail-emphasize" ] [ HH.text location ]
    ]

playerLanguagesDetail :: forall slots action. Array String -> Maybe (HH.HTML slots action)
playerLanguagesDetail languages = arrangedAndDetail "fas fa-comments" "Speaks" languages

playerMicrophoneDetail :: forall slots action. Boolean -> Maybe (HH.HTML slots action)
playerMicrophoneDetail false = Nothing
playerMicrophoneDetail true = Just $
    detail "fas fa-microphone"
    [ HH.span [ HS.class_ "detail-labelless detail-emphasize" ]
        [ HH.text "Has microphone" ]
    , HH.text $ " and is willing to communicate"
    ]

playerDiscordTagDetail
    :: forall action left slots
    .  String
    -> Maybe String
    -> Maybe (HH.ComponentHTML action ( discordTag :: Copyable.Slot String | slots) (Async left))
playerDiscordTagDetail _ Nothing = Nothing
playerDiscordTagDetail nickname (Just discordTag) = Just $
    detail "fab fa-discord"
    [ HH.span [ HS.class_ "detail-labelless" ]
       [ copyable (SProxy :: SProxy "discordTag") ("discordTag-" <> nickname) discordTag ]
    ]
playerDetails :: forall fields action slots left.
    { nickname :: String
    , age :: Maybe Int
    , discordTag :: Maybe String
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
    -> Array (HH.ComponentHTML action (discordTag :: Copyable.Slot String | slots) (Async left))
playerDetails details =
    Array.catMaybes
    [ playerAgeDetail details.age
    , playerLocationDetail details.location
    , playerLanguagesDetail details.languages
    , playerMicrophoneDetail details.microphone
    , playerDiscordTagDetail details.nickname details.discordTag
    , weekdaysOnlineDetail details.weekdayOnline
    , weekendsOnlineDetail details.weekendOnline
    ]

module TeamTavern.Client.Pages.Team.Details (details) where

import Prelude

import Async (Async)
import Client.Components.Copyable as Copyable
import Data.Array (foldr, null)
import Data.Array as Array
import Data.Maybe (Maybe(..), isNothing)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Server.Team.View (Team)

noDetails :: Team -> Boolean
noDetails team =
    isNothing team.website && isNothing team.ageFrom && isNothing team.ageTo
    && null team.locations && null team.languages && not team.hasMicrophone
    && isNothing team.discordServer && isNothing team.clientWeekdayOnline
    && isNothing team.clientWeekendOnline

noAbout :: Team -> Boolean
noAbout team = null team.about

detail :: forall slots action. String -> Array (HH.HTML slots action) -> HH.HTML slots action
detail icon children =
    HH.p [ HS.class_ "profile-field" ] $
    [ HH.i [ HS.class_ $ icon <> " profile-field-icon" ] [] ]
    <> children

urlDetail :: forall slots action. String -> String -> Maybe String -> Maybe (HH.HTML slots action)
urlDetail _ _ Nothing = Nothing
urlDetail icon text (Just href) = Just $ detail icon
    [ HH.a [ HS.class_ "profile-field-url", HP.target "_blank", HP.href href ] [ HH.text text ] ]

teamWebsiteDetails :: forall slots action. Maybe String -> Maybe (HH.HTML slots action)
teamWebsiteDetails website = urlDetail "fas fa-globe" "Website" website

teamAgeDetail :: forall slots action. Maybe Int -> Maybe Int -> Maybe (HH.HTML slots action)
teamAgeDetail Nothing Nothing = Nothing
teamAgeDetail (Just from) Nothing = Just $
    detail "fas fa-calendar-alt"
    [ HH.span [ HS.class_ "profile-field-labelless" ] [ HH.text "Are older than " ]
    , HH.span [ HS.class_ "profile-field-emphasize" ] [ HH.text $ show from ]
    ]
teamAgeDetail Nothing (Just to) = Just $
    detail "fas fa-calendar-alt"
    [ HH.span [ HS.class_ "profile-field-labelless" ] [ HH.text "Are younger than " ]
    , HH.span [ HS.class_ "profile-field-emphasize" ] [ HH.text $ show to ]
    ]
teamAgeDetail (Just from) (Just to) = Just $
    detail "fas fa-calendar-alt"
    [ HH.span [ HS.class_ "profile-field-labelless" ] [ HH.text "Are between " ]
    , HH.span [ HS.class_ "profile-field-emphasize" ] [ HH.text $ show from ]
    , HH.text " and "
    , HH.span [ HS.class_ "profile-field-emphasize" ] [ HH.text $ show to ]
    , HH.text " years old"
    ]

arrangeItems :: forall slots action. Array String -> Array (HH.HTML slots action)
arrangeItems items =
    foldr
    (\item state ->
        if not state.firstItem
        then state { firstItem = true, itemsSoFar = [ HH.span [ HS.class_ "profile-field-emphasize" ] [ HH.text item ] ] }
        else if not state.secondItem
        then state { secondItem = true, itemsSoFar = [ HH.span [ HS.class_ "profile-field-emphasize" ] [ HH.text item ], HH.text " or " ] <> state.itemsSoFar }
        else state { itemsSoFar = [ HH.span [ HS.class_ "profile-field-emphasize" ] [ HH.text item ], HH.text ", " ] <> state.itemsSoFar }
    )
    { firstItem: false, secondItem: false, itemsSoFar: [] }
    items
    # _.itemsSoFar

arrangedDetail :: forall slots action. String -> String -> Array String -> Maybe (HH.HTML slots action)
arrangedDetail _ _ items | Array.null items = Nothing
arrangedDetail icon prefix items = Just $
    detail icon $
    [ HH.span [ HS.class_ "profile-field-labelless" ] [ HH.text $ prefix <> " " ] ]
    <> arrangeItems items

teamLocationsDetail :: forall t228 t229. Array String -> Maybe (HH.HTML t229 t228)
teamLocationsDetail locations = arrangedDetail "fas fa-globe-europe" "Live in" locations

teamLanguagesDetail :: forall t224 t225. Array String -> Maybe (HH.HTML t225 t224)
teamLanguagesDetail languages = arrangedDetail "fas fa-comments" "Speak" languages

teamMicrophoneDetail :: forall slots actions. Boolean -> Maybe (HH.HTML slots actions)
teamMicrophoneDetail false = Nothing
teamMicrophoneDetail true = Just $
    detail "fas fa-microphone"
    [ HH.span [ HS.class_ "profile-field-labelless profile-field-emphasize" ] [ HH.text "Have a microphone" ]
    , HH.text $ " and are willing to communicate"
    ]

teamDiscordServerDetail :: forall slots actions. Maybe String -> Maybe (HH.HTML slots actions)
teamDiscordServerDetail discordServer = urlDetail "fab fa-discord" "Dicord server" discordServer

onlineDetail :: forall slots actions. String -> Maybe { from :: String, to :: String } -> Maybe (HH.HTML slots actions)
onlineDetail _ Nothing = Nothing
onlineDetail frame (Just { from, to }) = Just $
    detail "fas fa-clock"
    [ HH.span [ HS.class_ "profile-field-labelless" ] [ HH.text $ "Online on " ]
    , HH.span [ HS.class_ "profile-field-emphasize" ] [ HH.text frame ]
    , HH.text " from "
    , HH.span [ HS.class_ "profile-field-emphasize" ] [ HH.text from ]
    , HH.text " to "
    , HH.span [ HS.class_ "profile-field-emphasize" ] [ HH.text to ]
    ]

weekdaysOnlineDetail :: forall slots actions. Maybe { from :: String, to :: String } -> Maybe (HH.HTML slots actions)
weekdaysOnlineDetail fromTo = onlineDetail "weekdays" fromTo

weekendsOnlineDetail :: forall slots actions. Maybe { from :: String, to :: String } -> Maybe (HH.HTML slots actions)
weekendsOnlineDetail fromTo = onlineDetail "weekends" fromTo

teamDetailsColumn :: forall slots action. Team -> Array (HH.HTML slots action)
teamDetailsColumn team | noDetails team = []
teamDetailsColumn team = Array.singleton $
    HH.div [ HS.class_ "profile-column" ] $
    [ HH.h4 [ HS.class_ "player-profile-section-title" ] [ HH.text "Team details" ] ]
    <> Array.catMaybes
    [ teamWebsiteDetails team.website
    , teamAgeDetail team.ageFrom team.ageTo
    , teamLocationsDetail team.locations
    , teamLanguagesDetail team.languages
    , teamMicrophoneDetail team.hasMicrophone
    , teamDiscordServerDetail team.discordServer
    , weekdaysOnlineDetail team.clientWeekdayOnline
    , weekendsOnlineDetail team.clientWeekendOnline
    ]

teamAboutColumn :: forall slots action. Team -> Array (HH.HTML slots action)
teamAboutColumn team | noAbout team = []
teamAboutColumn team = Array.singleton $
    HH.div [ HS.class_ "profile-column" ] $
    [ HH.h4 [ HS.class_ "player-profile-section-title" ] [ HH.text "About" ] ]
    <> (team.about <#> \paragraph ->
        HH.p [ HS.class_ "profile-summary" ] [ HH.text paragraph ])

details
    :: forall action children left
    .  Team
    -> H.ComponentHTML
        action (discordServer :: Copyable.Slot | children) (Async left)
details team =
    HH.div [ HS.class_ "card" ] $
    [ HH.h2 [ HS.class_ "card-title" ] $
        [ HH.span [ HS.class_ "card-title-text" ]
            [ HH.text "Details" ]
        ]
    , HH.div [ HS.class_ "card-section" ]
        if noDetails team && noAbout team
        then [ HH.p_ [ HH.text "No details, kek." ] ]
        else
        [ HH.div [ HS.class_ "profile-columns details-container" ] $
            teamDetailsColumn team <> teamAboutColumn team
        ]
    ]

module TeamTavern.Client.Pages.Team.Details (details) where

import Prelude

import Async (Async)
import Client.Components.Copyable (copyable)
import Client.Components.Copyable as Copyable
import Data.Array (foldr, null)
import Data.Array as Array
import Data.Maybe (Maybe(..), isNothing)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Server.Team.View (Team)

noDetails :: Team -> Boolean
noDetails team =
    isNothing team.website && isNothing team.ageFrom && isNothing team.ageTo
    && null team.locations && null team.languages && not team.hasMicrophone
    && isNothing team.discordServer && isNothing team.clientWeekdayOnline
    && isNothing team.clientWeekendOnline

noAbout :: Team -> Boolean
noAbout team = null team.about

details
    :: forall action children left
    .  Team
    -> H.ComponentHTML
        action (discordServer :: Copyable.Slot | children) (Async left)
details team =
    HH.div [ HP.class_ $ HH.ClassName "card" ] $
    [ HH.h2 [ HP.class_ $ HH.ClassName "card-title" ] $
        [ HH.span [ HP.class_ $ HH.ClassName "card-title-text" ]
            [ HH.text "Details" ]
        ]
    , HH.div [ HP.class_ $ HH.ClassName "card-section" ]
        if noDetails team && noAbout team
        then [ HH.p_ [ HH.text "No details, kek." ] ]
        else
        [ HH.div [ HP.class_ $ HH.ClassName "profile-columns details-container" ] $
            (if noDetails team
            then []
            else
            [ HH.div [ HP.class_ $ HH.ClassName "profile-column" ] $
                [ HH.h4 [ HP.class_ $ HH.ClassName "player-profile-section-title" ] [ HH.text "Team details" ] ]
                <> Array.catMaybes
                [ case team.ageFrom, team.ageTo of
                    Nothing, Nothing -> Nothing
                    Just from, Nothing -> Just $
                        HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-calendar-alt profile-field-icon" ] []
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Are older than " ]
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text $ show from ]
                        ]
                    Nothing, Just to -> Just $
                        HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-calendar-alt profile-field-icon" ] []
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Are younger than " ]
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text $ show to ]
                        ]
                    Just from, Just to -> Just $
                        HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-calendar-alt profile-field-icon" ] []
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Are between " ]
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text $ show from ]
                        , HH.text " and "
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text $ show to ]
                        , HH.text " years old"
                        ]
                , if Array.null team.locations
                    then Nothing
                    else Just $
                        HH.p [ HP.class_ $ HH.ClassName "profile-field" ] $
                        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-globe-europe profile-field-icon" ] []
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Live in " ]
                        ]
                        <>
                        (foldr
                            (\country state ->
                                if not state.firstCountry
                                then state { firstCountry = true, regionsSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text country ] ] }
                                else if not state.secondCountry
                                then state { secondCountry = true, regionsSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text country ], HH.text " or " ] <> state.regionsSoFar }
                                else state { regionsSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text country ], HH.text ", " ] <> state.regionsSoFar }
                            )
                            { firstCountry: false, secondCountry: false, regionsSoFar: [] }
                            team.locations
                            # _.regionsSoFar)
                , if Array.null team.languages
                    then Nothing
                    else Just $
                        HH.p [ HP.class_ $ HH.ClassName "profile-field" ] $
                        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-comments profile-field-icon" ] []
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Speak " ]
                        ]
                        <>
                        (foldr
                            (\language state ->
                                if not state.firstLanguage
                                then state { firstLanguage = true, languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ] ] }
                                else if not state.secondLanguage
                                then state { secondLanguage = true, languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ], HH.text " or " ] <> state.languagesSoFar }
                                else state { languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ], HH.text ", " ] <> state.languagesSoFar }
                            )
                            { firstLanguage: false, secondLanguage: false, languagesSoFar: [] }
                            team.languages
                            # _.languagesSoFar)
                , if team.hasMicrophone
                    then Just $
                        HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-microphone profile-field-icon" ] []
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless profile-field-emphasize" ] [ HH.text "Have a microphone" ]
                        , HH.text $ " and are willing to communicate"
                        ]
                    else Nothing
                , team.discordServer <#> \discordServer ->
                    HH.p [ HP.class_ $ HH.ClassName "profile-field" ] $
                    [ HH.i [ HP.class_ $ HH.ClassName "fab fa-discord profile-field-icon" ] []
                    , HH.span [ HP.class_ $ HH.ClassName "profile-field-label" ] [ HH.text "Discord tag: " ]
                    , copyable (SProxy :: SProxy "discordServer") discordServer
                    ]
                , team.clientWeekdayOnline <#> \{ from, to } ->
                    HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                    [ HH.i [ HP.class_ $ HH.ClassName "fas fa-clock profile-field-icon" ] []
                    , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text $ "Online on " ]
                    , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text "weekdays" ]
                    , HH.text " from "
                    , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text from ]
                    , HH.text " to "
                    , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text to ]
                    ]
                , team.clientWeekendOnline <#> \{ from, to } ->
                    HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                    [ HH.i [ HP.class_ $ HH.ClassName "fas fa-clock profile-field-icon" ] []
                    , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text $ "Online on " ]
                    , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text "weekends" ]
                    , HH.text " from "
                    , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text from ]
                    , HH.text " to "
                    , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text to ]
                    ]
                ]
            ])
            <>
            if noAbout team
            then []
            else
            [ HH.div [ HP.class_ $ HH.ClassName "profile-column" ] $
                [ HH.h4 [ HP.class_ $ HH.ClassName "player-profile-section-title" ] [ HH.text "About" ] ]
                <> (team.about <#> \paragraph ->
                    HH.p [ HP.class_ $ HH.ClassName "profile-summary" ] [ HH.text paragraph ])
            ]

        ]
    ]

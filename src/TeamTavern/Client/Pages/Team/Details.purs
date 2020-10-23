module TeamTavern.Client.Pages.Team.Details (details) where

import Prelude

import Async (Async)
import Client.Components.Copyable as Copyable
import Data.Array (null)
import Data.Array as Array
import Data.Maybe (Maybe(..), isNothing)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Record as Record
import TeamTavern.Client.Components.Detail (textDetail)
import TeamTavern.Client.Components.Team.TeamDetail (teamDetails)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Server.Team.View (Team)

noDetails :: Team -> Boolean
noDetails team =
    isNothing team.website && isNothing team.ageFrom && isNothing team.ageTo && null team.locations
    && null team.languages && not team.microphone && isNothing team.discordServer
    && isNothing team.weekdayOnline && isNothing team.weekendOnline

noAbout :: Team -> Boolean
noAbout team = null team.about

teamDetailsColumn :: forall slots action. Team -> Array (HH.HTML slots action)
teamDetailsColumn team | noDetails team = []
teamDetailsColumn team = Array.singleton $
    HH.div [ HS.class_ "profile-column" ] $
    [ HH.h4 [ HS.class_ "player-profile-section-title" ] [ HH.text "Team details" ] ]
    <> teamDetails
        ( team
            # Record.modify (SProxy :: SProxy "weekdayOnline")
                case _ of
                Just { clientFrom, clientTo } -> Just { from: clientFrom, to: clientTo }
                Nothing -> Nothing
            # Record.modify (SProxy :: SProxy "weekendOnline")
                case _ of
                Just { clientFrom, clientTo } -> Just { from: clientFrom, to: clientTo }
                Nothing -> Nothing
        )

teamAboutColumn :: forall slots action. Team -> Array (HH.HTML slots action)
teamAboutColumn team | noAbout team = []
teamAboutColumn team = Array.singleton $
    HH.div [ HS.class_ "profile-column" ] $
    [ HH.h4 [ HS.class_ "player-profile-section-title" ] [ HH.text "About" ] ]
    <> textDetail team.about

details :: forall action children left.
    Team -> H.ComponentHTML action (discordServer :: Copyable.Slot | children) (Async left)
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

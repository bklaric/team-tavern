module TeamTavern.Client.Pages.Player.TeamProfiles (Slot, teamProfiles) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (intercalate)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Halogen (defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Pages.Player.EditTeamProfile (editTeamProfile)
import TeamTavern.Client.Pages.Player.EditTeamProfile as EditProfile
import TeamTavern.Client.Pages.Player.Types (Nickname, PlayerStatus(..))
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Server.Profile.ViewTeamProfilesByTeam.SendResponse as ViewTeamProfilesByPlayer

data Input = Input Nickname PlayerStatus

data Action
    = Initialize
    | Receive Input
    | ShowModal EditProfile.Input
    | HandleModalOutput (Modal.Message EditProfile.Output)

data State
    = Empty Input
    | Profiles Nickname PlayerStatus ViewTeamProfilesByPlayer.OkContent

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots =
    ( games :: Anchor.Slot Int
    , editProfile :: EditProfile.Slot Unit
    )

yearSeconds :: Number
yearSeconds = 60.0 * 60.0 * 24.0 * 365.0

monthSeconds :: Number
monthSeconds = 60.0 * 60.0 * 24.0 * 30.0

daySeconds :: Number
daySeconds = 60.0 * 60.0 * 24.0

hourSeconds :: Number
hourSeconds = 60.0 * 60.0

minuteSeconds :: Number
minuteSeconds = 60.0

lastUpdated :: Number -> String
lastUpdated updatedSeconds = let
    yearsAgo = floor(updatedSeconds / yearSeconds)
    monthsAgo = floor(updatedSeconds / monthSeconds)
    daysAgo = floor(updatedSeconds / daySeconds)
    hoursAgo = floor(updatedSeconds / hourSeconds)
    minutesAgo = floor(updatedSeconds / minuteSeconds)
    interval =
        if yearsAgo > 0 then Just { unit: "year", count: yearsAgo } else
        if monthsAgo > 0 then Just { unit: "month", count: monthsAgo } else
        if daysAgo > 0 then Just { unit: "day", count: daysAgo } else
        if hoursAgo > 0 then Just { unit: "hour", count: hoursAgo } else
        if minutesAgo > 0 then Just { unit: "minute", count: minutesAgo } else
        Nothing
    in
    case interval of
    Just { unit, count } -> show count <> " " <> unit <> (if count == 1 then "" else "s") <> " ago"
    Nothing -> "less than a minute ago"

modalInput ::
    Nickname -> ViewTeamProfilesByPlayer.OkContent' -> EditProfile.Input
modalInput
    nickname
    { handle, title, fields, fieldValues, summary
    , age, countries, languages, hasMicrophone
    , timezone, weekdayOnline, weekendOnline, newOrReturning
    } =
    { nickname, handle, title, fields, fieldValues, summary
    , age, countries, languages, hasMicrophone
    , timezone, weekdayOnline, weekendOnline, newOrReturning
    }

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Profiles nickname playerStatus profiles) =
    HH.div [ HP.class_ $ HH.ClassName "card" ] $
    [ HH.h2 [ HP.class_ $ HH.ClassName "card-title" ]
        [ HH.text "Team profiles" ]
    ]
    <>
    case playerStatus of
    SamePlayer -> [ editTeamProfile $ Just <<< HandleModalOutput ]
    _ -> []
    <>
    if Array.null profiles
    then Array.singleton $
        HH.div [ HP.class_ $ HH.ClassName "card-section" ]
        [ HH.p_
            [ HH.text
                case playerStatus of
                SamePlayer -> "You haven't created any team profiles."
                _ -> "This player hasn't created any team profiles."
            ]
        ]
    else (profiles # Array.mapWithIndex \index profile ->
        HH.div [ HP.class_ $ HH.ClassName "card-section" ] $
        [ HH.h3 [ HP.class_ $ HH.ClassName "profile-title" ] $
            [ navigationAnchorIndexed (SProxy :: SProxy "games") index
                { path: "/games/" <> profile.handle <> "/players"
                , content: HH.text profile.title
                }
            ]
            <>
            case playerStatus of
            SamePlayer -> Array.singleton $
                HH.button
                [ HP.class_ $ HH.ClassName "regular-button profile-title-button"
                , HE.onClick $ const $ Just $ ShowModal $ modalInput nickname profile
                ]
                [ HH.i [ HP.class_ $ H.ClassName "fas fa-user-edit button-icon" ] []
                , HH.text "Edit team profile"
                ]
            _ -> []
            <>
            [ divider
            , HH.span [ HP.class_ $ HH.ClassName "profile-updated" ]
                [ HH.text $ "Updated " <> lastUpdated profile.updatedSeconds ]
            ]
        ]
        <> Array.catMaybes
        [ case profile.age.from, profile.age.to of
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
        , if Array.null profile.countries
            then Nothing
            else Just $
                HH.p [ HP.class_ $ HH.ClassName "profile-field" ] $
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-globe-europe profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Live in " ]
                ]
                <>
                (Array.foldr
                    (\country state ->
                        if not state.firstCountry
                        then state { firstCountry = true, regionsSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text country ] ] }
                        else if not state.secondCountry
                        then state { secondCountry = true, regionsSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text country ], HH.text " or " ] <> state.regionsSoFar }
                        else state { regionsSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text country ], HH.text ", " ] <> state.regionsSoFar }
                    )
                    { firstCountry: false, secondCountry: false, regionsSoFar: [] }
                    profile.countries
                    # _.regionsSoFar
                )
        , if Array.null profile.languages
            then Nothing
            else Just $
                HH.p [ HP.class_ $ HH.ClassName "profile-field" ] $
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-comments profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Speak " ]
                ]
                <>
                (Array.foldr
                    (\language state ->
                        if not state.firstLanguage
                        then state { firstLanguage = true, languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ] ] }
                        else if not state.secondLanguage
                        then state { secondLanguage = true, languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ], HH.text " or " ] <> state.languagesSoFar }
                        else state { languagesSoFar = [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text language ], HH.text ", " ] <> state.languagesSoFar }
                    )
                    { firstLanguage: false, secondLanguage: false, languagesSoFar: [] }
                    profile.languages
                    # _.languagesSoFar
                )
        , if profile.hasMicrophone
            then Just $
                HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-microphone profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless profile-field-emphasize" ] [ HH.text "Have a microphone" ]
                , HH.text $ " and are willing to communicate"
                ]
            else Nothing
        , profile.weekdayOnline <#> \{ clientFrom, clientTo } ->
            HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fas fa-clock profile-field-icon" ] []
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text $ "Online on " ]
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text "weekdays" ]
            , HH.text " from "
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text clientFrom ]
            , HH.text " to "
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text clientTo ]
            ]
        , profile.weekendOnline <#> \{ clientFrom, clientTo } ->
            HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
            [ HH.i [ HP.class_ $ HH.ClassName "fas fa-clock profile-field-icon" ] []
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text $ "Online on " ]
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text "weekends" ]
            , HH.text " from "
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text clientFrom ]
            , HH.text " to "
            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text clientTo ]
            ]
        ]
        <> Array.catMaybes
        (profile.fields <#> \field -> let
            fieldValue = profile.fieldValues # Array.find \{ fieldKey } -> field.key == fieldKey
            in
            case fieldValue of
            Just { optionKeys } -> let
                fieldOptions = field.options # Array.filter \{ key } -> Array.elem key optionKeys
                in
                if not $ Array.null fieldOptions
                then Just $
                    HH.p [ HP.class_ $ HH.ClassName "profile-field" ] $
                    [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                    , HH.span [ HP.class_ $ HH.ClassName "profile-field-label" ] [ HH.text $ field.label <> ": " ]
                    ]
                    <>
                    (intercalate [(HH.text ", ")] $
                        map (\{ label } -> [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text label ] ]) fieldOptions)
                else Nothing
            _ -> Nothing
        )
        <> (if profile.newOrReturning
            then Array.singleton $
                HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-book profile-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Are"]
                , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text " new or returning players" ]
                , HH.text $ " to the game"
                ]
            else [])
        <>
        (profile.summary <#> \paragraph -> HH.p_ [ HH.text paragraph ])
    )

loadProfiles :: forall left.
    String -> Async left (Maybe ViewTeamProfilesByPlayer.OkContent)
loadProfiles nickname = Async.unify do
    timezone <- H.liftEffect $ getClientTimezone
    response
        <- Fetch.fetch_ ("/api/profiles/by-nickname/" <> nickname <> "/teams?timezone=" <> timezone)
        #  lmap (const Nothing)
    content <-
        case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Nothing)
        _   -> Async.left Nothing
    pure $ Just content

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    case state of
        Empty (Input nickname playerStatus) -> do
            profiles <- H.lift $ loadProfiles nickname
            case profiles of
                Just profiles' ->
                    H.put $ Profiles nickname playerStatus profiles'
                Nothing -> pure unit
        _ -> pure unit
handleAction (Receive (Input nickname playerStatus)) = do
    state <- H.get
    case state of
        Profiles currentNickname _ _ | nickname == currentNickname -> pure unit
        _ -> do
            profiles <- H.lift $ loadProfiles nickname
            case profiles of
                Just profiles' -> H.put $ Profiles nickname playerStatus profiles'
                Nothing -> pure unit
handleAction (ShowModal profile) =
    Modal.showWith profile (SProxy :: SProxy "editProfile")
handleAction (HandleModalOutput message) = do
    Modal.hide (SProxy :: SProxy "editProfile")
    case message of
        Modal.Inner (EditProfile.ProfileUpdated nickname) ->
            handleAction $ Receive $ Input nickname SamePlayer
        _ -> pure unit

component :: forall output left query.
    H.Component HH.HTML query Input output (Async left)
component = mkComponent
    { initialState: Empty
    , render
    , eval: mkEval $ defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

teamProfiles
    :: forall children' name children action left
    .  Cons name (Slot) children' children
    => IsSymbol name
    => Nickname
    -> PlayerStatus
    -> SProxy name
    -> HH.ComponentHTML action children (Async left)
teamProfiles nickname playerStatus slot =
    HH.slot slot unit component (Input nickname playerStatus) absurd

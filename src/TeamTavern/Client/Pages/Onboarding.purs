module TeamTavern.Client.Pages.Onboarding (Step(..), PlayerOrTeam(..), Input, Slot, emptyInput, onboarding) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (foldl)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Options ((:=))
import Data.Symbol (SProxy(..))
import Data.Variant (match)
import Effect.Class (class MonadEffect)
import Foreign (ForeignError(..), fail, readString, unsafeToForeign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record
import Simple.JSON (class ReadForeign, class WriteForeign)
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.Button (primaryButton_, secondaryButton_)
import TeamTavern.Client.Components.Player.PlayerFormInput as PlayerFormInput
import TeamTavern.Client.Components.Player.ProfileFormInput as PlayerProfileFormInput
import TeamTavern.Client.Components.RadioButton (radioButton, radioButtons)
import TeamTavern.Client.Components.Team.ProfileFormInput as TeamProfileFormInput
import TeamTavern.Client.Components.Team.TeamFormInput as TeamFormInput
import TeamTavern.Client.Pages.Onboarding.SelectGame (selectGame)
import TeamTavern.Client.Pages.Onboarding.SelectGame as SelectGame
import TeamTavern.Client.Script.Cookie (getPlayerNickname)
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Navigate (navigate, navigateReplace, navigate_)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Onboarding as Onboarding
import Type (type ($))

data Step
    = Greeting
    | PlayerOrTeam
    | PlayerDetails
    | TeamDetails
    | Game
    | PlayerProfileDetails
    | TeamProfileDetails

instance writeForeginStep :: WriteForeign Step where
    writeImpl Greeting = unsafeToForeign "Greeting"
    writeImpl PlayerOrTeam = unsafeToForeign "PlayerOrTeam"
    writeImpl PlayerDetails = unsafeToForeign "PlayerDetails"
    writeImpl TeamDetails = unsafeToForeign "TeamDetails"
    writeImpl Game = unsafeToForeign "Game"
    writeImpl PlayerProfileDetails = unsafeToForeign "PlayerProfileDetails"
    writeImpl TeamProfileDetails = unsafeToForeign "TeamProfileDetails"

instance readForeignStep :: ReadForeign Step where
    readImpl = readString >=> case _ of
        "Greeting" -> pure Greeting
        "PlayerOrTeam" -> pure PlayerOrTeam
        "PlayerDetails" -> pure PlayerDetails
        "TeamDetails" -> pure TeamDetails
        "Game" -> pure Game
        "PlayerProfileDetails" -> pure PlayerProfileDetails
        "TeamProfileDetails" -> pure TeamProfileDetails
        step -> fail $ ForeignError $ "Unknown step " <> step

data PlayerOrTeam = Player | Team

instance writeForeignPlayerOrTeam :: WriteForeign PlayerOrTeam where
    writeImpl Player = unsafeToForeign "Player"
    writeImpl Team = unsafeToForeign "Team"

instance readForeignPlayerOrTeam :: ReadForeign PlayerOrTeam where
    readImpl = readString >=> case _ of
        "Player" -> pure Player
        "Team" -> pure Team
        playerOrTeam -> fail $ ForeignError $ "Unknown player or team " <> playerOrTeam

isPlayer :: PlayerOrTeam -> Boolean
isPlayer Player = true
isPlayer Team = false

isTeam :: PlayerOrTeam -> Boolean
isTeam Player = false
isTeam Team = true

type Input =
    { step :: Step
    , nickname :: String
    , playerOrTeam :: Maybe PlayerOrTeam
    , playerDetails :: PlayerFormInput.Input
    , teamDetails :: TeamFormInput.Input
    , game :: SelectGame.Input
    , playerProfileDetails :: PlayerProfileFormInput.Input
    , teamProfileDetails :: TeamProfileFormInput.Input
    , otherError :: Boolean
    }

emptyInput :: Input
emptyInput =
    { step: Greeting
    , nickname: ""
    , playerOrTeam: Nothing
    , playerDetails: PlayerFormInput.emptyInput
    , teamDetails: TeamFormInput.emptyInput
    , game: Nothing
    , playerProfileDetails: PlayerProfileFormInput.emptyInput []
    , teamProfileDetails: TeamProfileFormInput.emptyInput []
    , otherError: false
    }

type State =
    { step :: Step
    , confirmSkip :: Boolean
    , nickname :: String
    , playerOrTeam :: Maybe PlayerOrTeam
    , playerDetails :: PlayerFormInput.Input
    , teamDetails :: TeamFormInput.Input
    , game :: SelectGame.Input
    , playerProfileDetails :: PlayerProfileFormInput.Input
    , teamProfileDetails :: TeamProfileFormInput.Input
    , otherError :: Boolean
    , submitting :: Boolean
    }

data Action
    = Initialize
    | Receive Input
    | Skip
    | ConfirmSkip
    | SetStep Step
    | UpdatePlayerOrTeam PlayerOrTeam
    | UpdatePlayerDetails PlayerFormInput.Output
    | UpdateTeamDetails TeamFormInput.Output
    | UpdateGame SelectGame.Output
    | UpdatePlayerProfileDetails PlayerProfileFormInput.Output
    | UpdateTeamProfileDetails TeamProfileFormInput.Output
    | SetUpAccount

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots slots =
    ( playerFormInput :: PlayerFormInput.Slot
    , teamFormInput :: TeamFormInput.Slot
    , selectGame :: SelectGame.Slot
    , playerProfileFormInput :: PlayerProfileFormInput.Slot
    , teamProfileFormInput :: TeamProfileFormInput.Slot
    | slots )

onboarding' :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
onboarding' = HH.div [ HS.class_ "onboarding"]

onboardingStep :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
onboardingStep = HH.div [ HS.class_ "onboarding-step" ]

onboardingHeading :: forall slots action. String -> HH.HTML slots action
onboardingHeading text = HH.h1 [ HS.class_ "onboarding-heading" ] [ HH.text text ]

onboardingDescription :: forall slots action. String -> HH.HTML slots action
onboardingDescription text = HH.p [ HS.class_ "onboarding-description" ] [ HH.text text ]

onboardingButtons :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
onboardingButtons = HH.div [ HS.class_ "onboarding-buttons" ]

renderPage :: forall slots left.
    State -> Array (HH.ComponentHTML Action (ChildSlots slots) (Async left))
renderPage { step: Greeting, nickname, confirmSkip } =
    [ onboardingStep
        [ onboardingHeading $ "Hi, " <> nickname <> "!"
        , onboardingDescription  """Welcome to TeamTavern. Let's start with setting up your
                account and your first game profile."""
        ]
    , onboardingButtons
        [ HH.div [ HS.class_ "onboarding-skip-button-group" ]
            if confirmSkip
            then
                [ secondaryButton_ "Yes, I'm sure" ConfirmSkip
                , HH.p [ HS.class_ "onboarding-confirm-skip-button-underlabel" ]
                    [ HH.text "Are you sure? Setting up your account allows you to get the most out of TeamTavern." ]
                ]
            else
                [ secondaryButton_ "Skip" Skip
                , HH.p [ HS.class_ "onboarding-skip-button-underlabel" ]
                    [ HH.text "I don't want to set up", HH.br_, HH.text "my account right now." ]
                ]
        , primaryButton_ "Let's go" $ SetStep PlayerOrTeam
        ]
    ]
renderPage { step: PlayerOrTeam, playerOrTeam } =
    [ onboardingStep
        [ onboardingHeading "Player or team"
        , onboardingDescription "Do you want to create your own player profile or a team profile?"
        , radioButtons
            [ radioButton (maybe false isPlayer playerOrTeam) (UpdatePlayerOrTeam Player)
                [ HH.i [ HS.class_ "fas fa-user button-icon" ] []
                , HH.text "Create player profile"
                ]
            , radioButton (maybe false isTeam playerOrTeam) (UpdatePlayerOrTeam Team)
                [ HH.i [ HS.class_ "fas fa-users button-icon" ] []
                , HH.text "Create team profile"
                ]
            ]
        ]
    , onboardingButtons
        [ secondaryButton_ "Back" $ SetStep Greeting
        , HH.button
            [ HP.class_ $ HH.ClassName "primary-button"
            , HP.disabled $ isNothing playerOrTeam
            , HE.onClick $ const
                case playerOrTeam of
                Just Player -> Just $ SetStep PlayerDetails
                Just Team -> Just $ SetStep TeamDetails
                Nothing -> Nothing
            ]
            [ HH.text "Next" ]
        ]
    ]
renderPage { step: PlayerDetails, playerDetails } =
    [ onboardingStep
        [ onboardingHeading "Player details"
        , onboardingDescription  """Enter details about yourself so your new bruh gamer friends
                can find you, bruh. Fill out as much as you can to ensure the
                bruhest gamers find you. All fields are optional, bruh."""
        , PlayerFormInput.playerFormInput playerDetails (Just <<< UpdatePlayerDetails)
        ]
    , onboardingButtons
        [ secondaryButton_ "Back" $ SetStep PlayerOrTeam
        , primaryButton_ "Next" $ SetStep Game
        ]
    ]
renderPage { step: TeamDetails, teamDetails } =
    [ onboardingStep
        [ onboardingHeading "Team details"
        , onboardingDescription  """Enter details about yourself so your new bruh gamer friends
                can find you, bruh. Fill out as much as you can to ensure the
                bruhest gamers find you. All fields are optional, bruh."""
        , TeamFormInput.teamFormInput teamDetails (Just <<< UpdateTeamDetails)
        ]
    , onboardingButtons
        [ secondaryButton_ "Back" $ SetStep PlayerOrTeam
        , primaryButton_ "Next" $ SetStep Game
        ]
    ]
renderPage { step: Game, game, playerOrTeam } =
    [ onboardingStep
        [ onboardingHeading "Game"
        , onboardingDescription  """Select a game to create your first profile muhfugga."""
        , selectGame game (Just <<< UpdateGame)
        ]
    , onboardingButtons
        [ secondaryButton_ "Back"
            case playerOrTeam of
            Just Player -> SetStep PlayerDetails
            Just Team -> SetStep TeamDetails
            Nothing -> SetStep PlayerOrTeam
        , HH.button
            [ HP.class_ $ HH.ClassName "primary-button"
            , HP.disabled $ isNothing game
            , HE.onClick $ const $ Just $ SetStep
                case playerOrTeam of
                Just Player -> PlayerProfileDetails
                Just Team -> TeamProfileDetails
                Nothing -> PlayerProfileDetails
            ]
            [ HH.text "Next" ]
        ]
    ]
renderPage { step: PlayerProfileDetails, playerProfileDetails, otherError, submitting } =
    [ onboardingStep
        [ onboardingHeading "Player profile details"
        , onboardingDescription  """Enter details about your gameplay. Fill out everything."""
        , PlayerProfileFormInput.profileFormInput playerProfileDetails UpdatePlayerProfileDetails
        ]
    , onboardingButtons
        [ secondaryButton_ "Back" $ SetStep Game
        , HH.div [ HS.class_ "onboarding-submit-button-group" ] $
            [ HH.button
                [ HS.class_ "primary-button"
                , HP.disabled submitting
                , HE.onClick $ const $ Just SetUpAccount
                ]
                [ HH.text if submitting then "Submitting..." else "Submit" ]
            ]
            <>
            if otherError
            then Array.singleton $
                HH.p [ HS.class_ "onboarding-submit-button-underlabel" ]
                [ HH.text "There has been an unexpected error setting up your account. Please try again later." ]
            else []
        ]
    ]
renderPage { step: TeamProfileDetails, teamProfileDetails, otherError, submitting } =
    [ onboardingStep
        [ onboardingHeading "Team profile details"
        , onboardingDescription  """Enter details about your gameplay. Fill out everything."""
        , TeamProfileFormInput.profileFormInput teamProfileDetails UpdateTeamProfileDetails
        ]
    , onboardingButtons
        [ secondaryButton_ "Back" $ SetStep Game
        , HH.div [ HS.class_ "onboarding-submit-button-group" ] $
            [ HH.button
                [ HS.class_ "primary-button"
                , HP.disabled submitting
                , HE.onClick $ const $ Just SetUpAccount
                ]
                [ HH.text if submitting then "Submitting..." else "Submit" ]
            ]
            <>
            if otherError
            then Array.singleton $
                HH.p [ HS.class_ "onboarding-submit-button-underlabel" ]
                [ HH.text "There has been an unexpected error setting up your account. Please try again later." ]
            else []
        ]
    ]

render :: forall slots left.
    State -> HH.ComponentHTML Action (ChildSlots slots) (Async left)
render state = onboarding' $ renderPage state

sendRequest :: forall left.
    State -> Async left (Maybe (Either Onboarding.BadContent (Maybe Onboarding.OkContent)))
sendRequest (state :: State) = Async.unify do
    (body :: Onboarding.RequestContent) <-
        case state of
        { playerOrTeam: Just Player
        , playerDetails: player
        , game: Just game
        , playerProfileDetails: profile
        } -> Async.right
            { ilk: 1
            , player: Just
                { birthday: player.birthday
                , location: player.location
                , languages: player.languages
                , microphone: player.microphone
                , discordTag: player.discordTag
                , timezone: player.timezone
                , weekdayFrom: player.weekdayFrom
                , weekdayTo: player.weekdayTo
                , weekendFrom: player.weekendFrom
                , weekendTo: player.weekendTo
                , about: player.about
                }
            , team: Nothing
            , gameHandle: game.handle
            , playerProfile: Just
                { fieldValues: profile.fieldValues
                , newOrReturning: profile.newOrReturning
                , ambitions: profile.ambitions
                }
            , teamProfile: Nothing
            }
        { playerOrTeam: Just Team
        , teamDetails: team
        , game: Just game
        , teamProfileDetails: profile
        } -> Async.right
            { ilk: 2
            , player: Nothing
            , team: Just
                { name: team.name
                , website: team.website
                , ageFrom: team.ageFrom
                , ageTo: team.ageTo
                , locations: team.locations
                , languages: team.languages
                , microphone: team.microphone
                , discordServer: team.discordServer
                , timezone: team.timezone
                , weekdayFrom: team.weekdayFrom
                , weekdayTo: team.weekdayTo
                , weekendFrom: team.weekendFrom
                , weekendTo: team.weekendTo
                , about: team.about
                }
            , gameHandle: game.handle
            , playerProfile: Nothing
            , teamProfile: Just
                { fieldValues: profile.fieldValues
                , newOrReturning: profile.newOrReturning
                , ambitions: profile.ambitions
                }
            }
        _ -> Async.left Nothing
    response <-
        Fetch.fetch "/api/onboarding"
        ( Fetch.method := POST
        <> Fetch.body := Json.writeJSON body
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const Nothing)
    content :: Either Onboarding.BadContent $ Maybe Onboarding.OkContent <-
        case FetchRes.status response of
        200 -> FetchRes.text response >>= JsonAsync.readJSON # bimap (const Nothing) (Right <<< Just)
        204 -> pure $ Right Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON # bimap (const Nothing) Left
        _ -> Async.left Nothing
    pure $ Just content

-- Update state for current history entry so back button doesn't lose previous state.
updateHistoryState :: forall monad. MonadEffect monad => State -> monad Unit
updateHistoryState (state :: State) = do
    case state.step of
        Greeting -> navigateReplace state "/onboarding/start"
        PlayerOrTeam -> navigateReplace state "/onboarding/player-or-team"
        PlayerDetails -> navigateReplace state "/onboarding/player"
        TeamDetails -> navigateReplace state "/onboarding/team"
        Game -> navigateReplace state "/onboarding/game"
        PlayerProfileDetails -> navigateReplace state "/onboarding/player-profile"
        TeamProfileDetails -> navigateReplace state "/onboarding/team-profile"

handleAction :: forall action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    -- case state.step of
    --     Greeting -> pure unit
    --     _ -> navigate_ "/"
    nickname <- getPlayerNickname
    case nickname of
        Just nickname' -> H.modify_ _ { nickname = nickname' }
        Nothing -> navigate_ "/"
    H.liftEffect do
        setMetaTitle "Onboarding | TeamTavern"
        setMetaDescription "TeamTavern onboarding."
        setMetaUrl
handleAction (Receive input) =
    H.put
        ( input
        # Record.insert (SProxy :: SProxy "confirmSkip") false
        # Record.insert (SProxy :: SProxy "submitting") false
        )
handleAction Skip =
    H.modify_ _ { confirmSkip = true }
handleAction ConfirmSkip =
    navigate_ "/"
handleAction (SetStep step) = do
    state <- H.get
    case step of
        Greeting -> navigate state "/onboarding/start"
        PlayerOrTeam -> navigate state "/onboarding/player-or-team"
        PlayerDetails -> navigate state "/onboarding/player"
        TeamDetails -> navigate state "/onboarding/team"
        Game -> navigate state "/onboarding/game"
        PlayerProfileDetails -> navigate state "/onboarding/player-profile"
        TeamProfileDetails -> navigate state "/onboarding/team-profile"
handleAction (UpdatePlayerOrTeam playerOrTeam) = do
    state <- H.modify _ { playerOrTeam = Just playerOrTeam }
    updateHistoryState state
handleAction (UpdatePlayerDetails details) = do
    state <- H.modify _
        { playerDetails
            { birthday = details.birthday
            , location = details.location
            , languages = details.languages
            , microphone = details.microphone
            , discordTag = details.discordTag
            , timezone = details.timezone
            , weekdayFrom = details.weekdayFrom
            , weekdayTo = details.weekdayTo
            , weekendFrom = details.weekendFrom
            , weekendTo = details.weekendTo
            , about = details.about
            }
        }
    updateHistoryState state
handleAction (UpdateTeamDetails details) = do
    state <- H.modify _
        { teamDetails
            { name = details.name
            , website = details.website
            , ageFrom = details.ageFrom
            , ageTo = details.ageTo
            , locations = details.locations
            , languages = details.languages
            , microphone = details.microphone
            , discordServer = details.discordServer
            , timezone = details.timezone
            , weekdayFrom = details.weekdayFrom
            , weekdayTo = details.weekdayTo
            , weekendFrom = details.weekendFrom
            , weekendTo = details.weekendTo
            , about = details.about
            }
        }
    updateHistoryState state
handleAction (UpdateGame game) = do
    state <- H.modify _
        { game = Just game
        , playerProfileDetails
            { fields = game.fields
            , fieldValues = []
            , newOrReturning = false
            , ambitions = ""
            }
        , teamProfileDetails
            { fields = game.fields # Array.mapMaybe
                case _ of
                { ilk, key, label, icon, options: Just options } | ilk == 2 || ilk == 3 ->
                    Just { key, label, icon, options }
                _ -> Nothing
            , fieldValues = []
            , newOrReturning = false
            , ambitions = ""
            }
        }
    updateHistoryState state
handleAction (UpdatePlayerProfileDetails details) = do
    state <- H.modify _
        { playerProfileDetails
            { fieldValues = details.fieldValues
            , newOrReturning = details.newOrReturning
            , ambitions = details.ambitions
            }
        }
    updateHistoryState state
handleAction (UpdateTeamProfileDetails details) = do
    state <- H.modify _
        { teamProfileDetails
            { fieldValues = details.fieldValues
            , newOrReturning = details.newOrReturning
            , ambitions = details.ambitions
            }
        }
    updateHistoryState state
handleAction SetUpAccount = do
    currentState <- H.modify _ { submitting = true }
    let nextState = currentState
            { submitting = false
            , playerDetails
                { discordTagError = false
                , aboutError = false
                }
            , teamDetails
                { nameError = false
                , websiteError = false
                , discordServerError = false
                , aboutError = false
                }
            , playerProfileDetails
                { urlErrors = []
                , missingErrors = []
                , ambitionsError = false
                }
            }
    response <- H.lift $ sendRequest currentState
    case response of
        Just (Right Nothing) -> navigate_ "/"
        Just (Right (Just { teamHandle })) -> navigate_ $ "/teams/" <> teamHandle
        Just (Left errors) -> H.put $
            foldl
            (\state error ->
                match
                { player:
                    foldl
                    (\state' error' ->
                        match
                        { discordTag: const $ state'
                            { step = PlayerDetails
                            , playerDetails { discordTagError = true }
                            }
                        , about: const $ state'
                            { step = PlayerDetails
                            , playerDetails { aboutError = true }
                            }
                        }
                        error'
                    )
                    state
                , team:
                    foldl
                    (\state' error' ->
                        match
                        { name: const $ state'
                            { step = TeamDetails
                            , teamDetails { nameError = true }
                            }
                        , website: const $ state'
                            { step = TeamDetails
                            , teamDetails { websiteError = true }
                            }
                        , discordServer: const $ state'
                            { step = TeamDetails
                            , teamDetails { discordServerError = true }
                            }
                        , about: const $ state'
                            { step = TeamDetails
                            , teamDetails { aboutError = true }
                            }
                        }
                        error'
                    )
                    state
                , playerProfile:
                    foldl
                    (\state' error' ->
                        match
                        { url: \{ key } -> state'
                            { playerProfileDetails
                                { urlErrors = Array.cons key state'.playerProfileDetails.urlErrors }
                            }
                        , missing: \{ key } -> state'
                            { playerProfileDetails
                                { missingErrors =
                                    Array.cons key state'.playerProfileDetails.missingErrors
                                }
                            }
                        , ambitions: const $ state'
                            { playerProfileDetails { ambitionsError = true } }
                        }
                        error'
                    )
                    state
                , teamProfile:
                    foldl
                    (\state' error' ->
                        match
                        { ambitions: const $ state'
                            { playerProfileDetails { ambitionsError = true } }
                        }
                        error'
                    )
                    state
                }
                error
            )
            (nextState { otherError = false })
            errors
        Nothing -> H.put nextState { otherError = true }

component :: forall query output left.
    H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState:
        Record.insert (SProxy :: SProxy "confirmSkip") false
        >>> Record.insert (SProxy :: SProxy "submitting") false
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

onboarding :: forall action slots left.
    Input -> HH.ComponentHTML action (onboarding :: Slot | slots) (Async left)
onboarding input = HH.slot (SProxy :: SProxy "onboarding") unit component input absurd

module TeamTavern.Client.Pages.Onboarding (Step(..), Input, Slot, emptyInput, onboarding) where

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
import Data.Maybe (Maybe(..), isNothing)
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
import TeamTavern.Client.Components.Boarding.Boarding (boarding, boardingButtons, boardingDescription, boardingHeading, boardingStep)
import TeamTavern.Client.Components.Boarding.PlayerOrTeamInput (playerOrTeamInput)
import TeamTavern.Client.Components.Boarding.PlayerOrTeamInput as PlayerOrTeamInput
import TeamTavern.Client.Components.Button (primaryButton_, secondaryButton_)
import TeamTavern.Client.Components.Player.PlayerFormInput as PlayerFormInput
import TeamTavern.Client.Components.Player.ProfileFormInput as PlayerProfileFormInput
import TeamTavern.Client.Components.Team.ProfileFormInput as TeamProfileFormInput
import TeamTavern.Client.Components.Team.TeamFormInput as TeamFormInput
import TeamTavern.Client.Components.Boarding.GameInput (gameInput)
import TeamTavern.Client.Components.Boarding.GameInput as GameInput
import TeamTavern.Client.Script.Cookie (getPlayerNickname)
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Navigate (navigate, navigateReplace, navigate_)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Onboard as Onboard

data Step
    = Greeting
    | PlayerOrTeam
    | Player
    | Team
    | Game
    | PlayerProfile
    | TeamProfile

instance writeForeginStep :: WriteForeign Step where
    writeImpl Greeting = unsafeToForeign "Greeting"
    writeImpl PlayerOrTeam = unsafeToForeign "PlayerOrTeam"
    writeImpl Player = unsafeToForeign "Player"
    writeImpl Team = unsafeToForeign "Team"
    writeImpl Game = unsafeToForeign "Game"
    writeImpl PlayerProfile = unsafeToForeign "PlayerProfile"
    writeImpl TeamProfile = unsafeToForeign "TeamProfile"

instance readForeignStep :: ReadForeign Step where
    readImpl = readString >=> case _ of
        "Greeting" -> pure Greeting
        "PlayerOrTeam" -> pure PlayerOrTeam
        "Player" -> pure Player
        "Team" -> pure Team
        "Game" -> pure Game
        "PlayerProfile" -> pure PlayerProfile
        "TeamProfile" -> pure TeamProfile
        step -> fail $ ForeignError $ "Unknown step " <> step

type Input =
    { step :: Step
    , nickname :: String
    , playerOrTeam :: Maybe PlayerOrTeamInput.PlayerOrTeam
    , player :: PlayerFormInput.Input
    , team :: TeamFormInput.Input
    , game :: GameInput.Input
    , playerProfile :: PlayerProfileFormInput.Input
    , teamProfile :: TeamProfileFormInput.Input
    , otherError :: Boolean
    }

emptyInput :: Input
emptyInput =
    { step: Greeting
    , nickname: ""
    , playerOrTeam: Nothing
    , player: PlayerFormInput.emptyInput
    , team: TeamFormInput.emptyInput
    , game: Nothing
    , playerProfile: PlayerProfileFormInput.emptyInput { externalIdIlk: 1, fields: [] }
    , teamProfile: TeamProfileFormInput.emptyInput []
    , otherError: false
    }

type State =
    { step :: Step
    , confirmSkip :: Boolean
    , nickname :: String
    , playerOrTeam :: Maybe PlayerOrTeamInput.PlayerOrTeam
    , player :: PlayerFormInput.Input
    , team :: TeamFormInput.Input
    , game :: GameInput.Input
    , playerProfile :: PlayerProfileFormInput.Input
    , teamProfile :: TeamProfileFormInput.Input
    , otherError :: Boolean
    , submitting :: Boolean
    }

data Action
    = Initialize
    | Receive Input
    | Skip
    | ConfirmSkip
    | SetStep Step
    | UpdatePlayerOrTeam PlayerOrTeamInput.PlayerOrTeam
    | UpdatePlayer PlayerFormInput.Output
    | UpdateTeam TeamFormInput.Output
    | UpdateGame GameInput.Output
    | UpdatePlayerProfile PlayerProfileFormInput.Output
    | UpdateTeamProfile TeamProfileFormInput.Output
    | SetUpAccount

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots slots =
    ( playerFormInput :: PlayerFormInput.Slot
    , teamFormInput :: TeamFormInput.Slot
    , gameInput :: GameInput.Slot
    , playerProfileFormInput :: PlayerProfileFormInput.Slot
    , teamProfileFormInput :: TeamProfileFormInput.Slot
    | slots )

renderPage :: forall slots left.
    State -> Array (HH.ComponentHTML Action (ChildSlots slots) (Async left))
renderPage { step: Greeting, nickname, confirmSkip } =
    [ boardingStep
        [ boardingHeading $ "Hi, " <> nickname <> "!"
        , boardingDescription  """Welcome to TeamTavern. Let's start with setting up your
                account and your first game profile."""
        ]
    , boardingButtons
        [ HH.div [ HS.class_ "boarding-skip-button-group" ]
            if confirmSkip
            then
                [ secondaryButton_ "Yes, I'm sure" ConfirmSkip
                , HH.p [ HS.class_ "boarding-confirm-skip-button-underlabel" ]
                    [ HH.text "Are you sure? Setting up your account allows you to get the most out of TeamTavern." ]
                ]
            else
                [ secondaryButton_ "Skip" Skip
                , HH.p [ HS.class_ "boarding-skip-button-underlabel" ]
                    [ HH.text "I don't want to set up", HH.br_, HH.text "my account right now." ]
                ]
        , primaryButton_ "Let's go" $ SetStep PlayerOrTeam
        ]
    ]
renderPage { step: PlayerOrTeam, playerOrTeam } =
    [ boardingStep
        [ boardingHeading "Player or team"
        , boardingDescription "Do you want to create your own player profile or a team profile?"
        , playerOrTeamInput playerOrTeam UpdatePlayerOrTeam
        ]
    , boardingButtons
        [ secondaryButton_ "Back" $ SetStep Greeting
        , HH.button
            [ HP.class_ $ HH.ClassName "primary-button"
            , HP.disabled $ isNothing playerOrTeam
            , HE.onClick $ const
                case playerOrTeam of
                Just PlayerOrTeamInput.Player -> Just $ SetStep Player
                Just PlayerOrTeamInput.Team -> Just $ SetStep Team
                Nothing -> Nothing
            ]
            [ HH.text "Next" ]
        ]
    ]
renderPage { step: Player, player } =
    [ boardingStep
        [ boardingHeading "Player"
        , boardingDescription  """Tell us about yourself. Fill out as much as you want to help us
            find you the right teammates. All fields are optional."""
        , PlayerFormInput.playerFormInput player (Just <<< UpdatePlayer)
        ]
    , boardingButtons
        [ secondaryButton_ "Back" $ SetStep PlayerOrTeam
        , primaryButton_ "Next" $ SetStep Game
        ]
    ]
renderPage { step: Team, team } =
    [ boardingStep
        [ boardingHeading "Team"
        , boardingDescription  """Tell us about your team. Fill out as much as you want to help us
            find the right teammates for your team. All fields are optional."""
        , TeamFormInput.teamFormInput team (Just <<< UpdateTeam)
        ]
    , boardingButtons
        [ secondaryButton_ "Back" $ SetStep PlayerOrTeam
        , primaryButton_ "Next" $ SetStep Game
        ]
    ]
renderPage { step: Game, game, playerOrTeam } =
    [ boardingStep
        [ boardingHeading "Game"
        , boardingDescription  """Select a game to create your first profile."""
        , gameInput game (Just <<< UpdateGame)
        ]
    , boardingButtons
        [ secondaryButton_ "Back"
            case playerOrTeam of
            Just PlayerOrTeamInput.Player -> SetStep Player
            Just PlayerOrTeamInput.Team -> SetStep Team
            Nothing -> SetStep PlayerOrTeam
        , HH.button
            [ HP.class_ $ HH.ClassName "primary-button"
            , HP.disabled $ isNothing game
            , HE.onClick $ const $ Just $ SetStep
                case playerOrTeam of
                Just PlayerOrTeamInput.Player -> PlayerProfile
                Just PlayerOrTeamInput.Team -> TeamProfile
                Nothing -> PlayerProfile
            ]
            [ HH.text "Next" ]
        ]
    ]
renderPage { step: PlayerProfile, playerProfile, otherError, submitting } =
    [ boardingStep
        [ boardingHeading "Player profile"
        , boardingDescription  """Fill out your in-game stats, achievements and ambitions to find
            equally skilled teammates."""
        , PlayerProfileFormInput.profileFormInput playerProfile UpdatePlayerProfile
        ]
    , boardingButtons
        [ secondaryButton_ "Back" $ SetStep Game
        , HH.div [ HS.class_ "boarding-submit-button-group" ] $
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
                HH.p [ HS.class_ "boarding-submit-button-underlabel" ]
                [ HH.text "There has been an unexpected error setting up your account. Please try again later." ]
            else []
        ]
    ]
renderPage { step: TeamProfile, teamProfile, otherError, submitting } =
    [ boardingStep
        [ boardingHeading "Team profile"
        , boardingDescription  """Tell us about your team's ambitions and what you're looking for
            skill-wise in new team members."""
        , TeamProfileFormInput.profileFormInput teamProfile UpdateTeamProfile
        ]
    , boardingButtons
        [ secondaryButton_ "Back" $ SetStep Game
        , HH.div [ HS.class_ "boarding-submit-button-group" ] $
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
                HH.p [ HS.class_ "boarding-submit-button-underlabel" ]
                [ HH.text "There has been an unexpected error setting up your account. Please try again later." ]
            else []
        ]
    ]

render :: forall slots left.
    State -> HH.ComponentHTML Action (ChildSlots slots) (Async left)
render state = boarding $ renderPage state

sendRequest :: forall left.
    State -> Async left (Maybe (Either Onboard.BadContent Onboard.OkContent))
sendRequest (state :: State) = Async.unify do
    (body :: Onboard.RequestContent) <-
        case state of
        { playerOrTeam: Just PlayerOrTeamInput.Player
        , player: player
        , game: Just game
        , playerProfile: profile
        } -> Async.right
            { ilk: 1
            , player: Just
                { birthday: player.birthday
                , location: player.location
                , languages: player.languages
                , microphone: player.microphone
                , timezone: player.timezone
                , weekdayFrom: player.weekdayFrom
                , weekdayTo: player.weekdayTo
                , weekendFrom: player.weekendFrom
                , weekendTo: player.weekendTo
                , discordTag: player.discordTag
                , steamUrl: player.steamUrl
                , riotId: player.riotId
                , about: player.about
                }
            , team: Nothing
            , gameHandle: game.handle
            , playerProfile: Just
                { externalId: profile.externalId
                , fieldValues: profile.fieldValues
                , newOrReturning: profile.newOrReturning
                , ambitions: profile.ambitions
                }
            , teamProfile: Nothing
            }
        { playerOrTeam: Just PlayerOrTeamInput.Team
        , team: team
        , game: Just game
        , teamProfile: profile
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
    content :: Either Onboard.BadContent Onboard.OkContent <-
        case FetchRes.status response of
        200 -> FetchRes.text response >>= JsonAsync.readJSON # bimap (const Nothing) Right
        400 -> FetchRes.text response >>= JsonAsync.readJSON # bimap (const Nothing) Left
        _ -> Async.left Nothing
    pure $ Just content

-- Update state for current history entry so back button doesn't lose previous state.
updateHistoryState :: forall monad. MonadEffect monad => State -> monad Unit
updateHistoryState (state :: State) = do
    case state.step of
        Greeting -> navigateReplace state "/onboarding/start"
        PlayerOrTeam -> navigateReplace state "/onboarding/player-or-team"
        Player -> navigateReplace state "/onboarding/player"
        Team -> navigateReplace state "/onboarding/team"
        Game -> navigateReplace state "/onboarding/game"
        PlayerProfile -> navigateReplace state "/onboarding/player-profile"
        TeamProfile -> navigateReplace state "/onboarding/team-profile"

handleAction :: forall action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction Initialize = do
    state <- H.get
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
        Player -> navigate state "/onboarding/player"
        Team -> navigate state "/onboarding/team"
        Game -> navigate state "/onboarding/game"
        PlayerProfile -> navigate state "/onboarding/player-profile"
        TeamProfile -> navigate state "/onboarding/team-profile"
handleAction (UpdatePlayerOrTeam playerOrTeam) = do
    state <- H.modify _ { playerOrTeam = Just playerOrTeam }
    updateHistoryState state
handleAction (UpdatePlayer details) = do
    state <- H.modify _
        { player
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
handleAction (UpdateTeam details) = do
    state <- H.modify _
        { team
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
        , playerProfile
            { externalIdIlk = game.externalIdIlk
            , fields = game.fields
            , externalId = ""
            , fieldValues = []
            , newOrReturning = false
            , ambitions = ""
            }
        , teamProfile
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
handleAction (UpdatePlayerProfile details) = do
    state <- H.modify _
        { playerProfile
            { externalId = details.externalId
            , fieldValues = details.fieldValues
            , newOrReturning = details.newOrReturning
            , ambitions = details.ambitions
            }
        }
    updateHistoryState state
handleAction (UpdateTeamProfile details) = do
    state <- H.modify _
        { teamProfile
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
            , player
                { discordTagError = false
                , aboutError = false
                }
            , team
                { nameError = false
                , websiteError = false
                , discordServerError = false
                , aboutError = false
                }
            , playerProfile
                { externalIdError = false
                , urlErrors = []
                , ambitionsError = false
                }
            }
    response <- H.lift $ sendRequest currentState
    case response of
        Just (Right { teamHandle: Nothing }) -> navigate_ "/"
        Just (Right { teamHandle: Just teamHandle}) -> navigate_ $ "/teams/" <> teamHandle
        Just (Left errors) -> H.put $
            foldl
            (\state error ->
                match
                { player:
                    foldl
                    (\state' error' ->
                        match
                        { discordTag: const $ state'
                            { step = Player
                            , player { discordTagError = true }
                            }
                        , steamUrl: const $ state'
                            { step = Player
                            , player { steamUrlError = true }
                            }
                        , riotId: const $ state'
                            { step = Player
                            , player { riotIdError = true }
                            }
                        , about: const $ state'
                            { step = Player
                            , player { aboutError = true }
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
                            { step = Team
                            , team { nameError = true }
                            }
                        , website: const $ state'
                            { step = Team
                            , team { websiteError = true }
                            }
                        , discordServer: const $ state'
                            { step = Team
                            , team { discordServerError = true }
                            }
                        , about: const $ state'
                            { step = Team
                            , team { aboutError = true }
                            }
                        }
                        error'
                    )
                    state
                , playerProfile:
                    foldl
                    (\state' error' ->
                        match
                        { externalId: const $ state'
                            { playerProfile { externalIdError = true } }
                        , url: \{ key } -> state'
                            { playerProfile
                                { urlErrors = Array.cons key state'.playerProfile.urlErrors }
                            }
                        , ambitions: const $ state'
                            { playerProfile { ambitionsError = true } }
                        }
                        error'
                    )
                    state
                , teamProfile:
                    foldl
                    (\state' error' ->
                        match
                        { ambitions: const $ state'
                            { playerProfile { ambitionsError = true } }
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

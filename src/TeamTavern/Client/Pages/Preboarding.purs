module TeamTavern.Client.Pages.Preboarding (Game(..), Step(..), Input, Slot, emptyInput, preboarding) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Control.Alt ((<|>))
import Data.Array (foldl, mapMaybe)
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
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.Boarding.Boarding (boarding, boardingButtons, boardingDescription, boardingHeading, boardingStep)
import TeamTavern.Client.Components.Boarding.GameInput (gameInput)
import TeamTavern.Client.Components.Boarding.GameInput as GameInput
import TeamTavern.Client.Components.Boarding.PlayerOrTeamInput as PlayerOrTeamInput
import TeamTavern.Client.Components.Button (primaryButton_, secondaryButton_)
import TeamTavern.Client.Components.Player.PlayerFormInput as PlayerFormInput
import TeamTavern.Client.Components.Player.ProfileFormInput as PlayerProfileFormInput
import TeamTavern.Client.Components.RegistrationInput (registrationInput)
import TeamTavern.Client.Components.RegistrationInput as RegistrationInput
import TeamTavern.Client.Components.Team.ProfileFormInput as TeamProfileFormInput
import TeamTavern.Client.Components.Team.TeamFormInput as TeamFormInput
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Navigate (navigate, navigateReplace, navigate_)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Preboarding as Preboarding
import TeamTavern.Server.Game.View.SendResponse as ViewGame
import Type (type ($))

data Step
    = Greeting
    | Player
    | Team
    | Game
    | PlayerProfile
    | TeamProfile
    | Register

derive instance eqStep :: Eq Step

derive instance ordStep :: Ord Step

instance writeForeginStep :: WriteForeign Step where
    writeImpl Greeting = unsafeToForeign "Greeting"
    writeImpl Player = unsafeToForeign "Player"
    writeImpl Team = unsafeToForeign "Team"
    writeImpl Game = unsafeToForeign "Game"
    writeImpl PlayerProfile = unsafeToForeign "PlayerProfile"
    writeImpl TeamProfile = unsafeToForeign "TeamProfile"
    writeImpl Register = unsafeToForeign "Register"

instance readForeignStep :: ReadForeign Step where
    readImpl = readString >=> case _ of
        "Greeting" -> pure Greeting
        "Player" -> pure Player
        "Team" -> pure Team
        "Game" -> pure Game
        "PlayerProfile" -> pure PlayerProfile
        "TeamProfile" -> pure TeamProfile
        "Register" -> pure Register
        step -> fail $ ForeignError $ "Unknown step " <> step

data Game = Preselected ViewGame.OkContent | Selected GameInput.Input

getGame :: Game -> Maybe ViewGame.OkContent
getGame (Preselected game) = Just game
getGame (Selected (Just game)) = Just game
getGame _ = Nothing

instance writeForeginGame :: WriteForeign Game where
    writeImpl (Preselected game) = writeImpl { preselected: game }
    writeImpl (Selected input) = writeImpl { selected: input }

instance readForeignGame :: ReadForeign Game where
    readImpl foreign' =
        ( (readImpl foreign' :: _ { preselected :: ViewGame.OkContent })
            <#> _.preselected <#> Preselected
        )
        <|>
        ( (readImpl foreign' :: _ { selected :: Maybe ViewGame.OkContent })
            <#> _.selected <#> Selected
        )

type Input =
    { step :: Step
    , playerOrTeam :: PlayerOrTeamInput.PlayerOrTeam
    , player :: PlayerFormInput.Input
    , team :: TeamFormInput.Input
    , game :: Game
    , playerProfile :: PlayerProfileFormInput.Input
    , teamProfile :: TeamProfileFormInput.Input
    , registration :: RegistrationInput.Input
    , otherError :: Boolean
    }

emptyInput :: PlayerOrTeamInput.PlayerOrTeam -> Maybe ViewGame.OkContent -> Input
emptyInput playerOrTeam game =
    { step: Greeting
    , playerOrTeam
    , player: PlayerFormInput.emptyInput
    , team: TeamFormInput.emptyInput
    , game:
        case game of
        Just game' -> Preselected game'
        Nothing -> Selected Nothing
    , playerProfile: PlayerProfileFormInput.emptyInput $ maybe [] _.fields game
    , teamProfile: TeamProfileFormInput.emptyInput $ maybe [] (_.fields >>>
        mapMaybe
        case _ of
        { ilk, key, label, icon, options: Just options } | ilk == 2 || ilk == 3 ->
            Just { key, label, icon, options: options }
        _ -> Nothing) game
    , registration: RegistrationInput.emptyInput
    , otherError: false
    }

type State =
    { step :: Step
    , playerOrTeam :: PlayerOrTeamInput.PlayerOrTeam
    , player :: PlayerFormInput.Input
    , team :: TeamFormInput.Input
    , game :: Game
    , playerProfile :: PlayerProfileFormInput.Input
    , teamProfile :: TeamProfileFormInput.Input
    , registration :: RegistrationInput.Input
    , otherError :: Boolean
    , submitting :: Boolean
    }

data Action
    = Initialize
    | Receive Input
    | Exit
    | SetStep Step
    | UpdatePlayer PlayerFormInput.Output
    | UpdateTeam TeamFormInput.Output
    | UpdateGame GameInput.Output
    | UpdatePlayerProfile PlayerProfileFormInput.Output
    | UpdateTeamProfile TeamProfileFormInput.Output
    | UpdateRegistration RegistrationInput.Output
    | SetUpAccount

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots slots =
    ( playerFormInput :: PlayerFormInput.Slot
    , teamFormInput :: TeamFormInput.Slot
    , gameInput :: GameInput.Slot
    , playerProfileFormInput :: PlayerProfileFormInput.Slot
    , teamProfileFormInput :: TeamProfileFormInput.Slot
    , registrationInput :: RegistrationInput.Slot
    | slots )

renderPage :: forall slots left.
    State -> Array (HH.ComponentHTML Action (ChildSlots slots) (Async left))
renderPage { step: Greeting, playerOrTeam } =
    [ boardingStep
        [ boardingHeading $ "Hi!"
        , boardingDescription  """Welcome to TeamTavern. Let's start with setting up your
                account and your first game profile."""
        ]
    , boardingButtons
        [ secondaryButton_ "Exit" Exit
        , primaryButton_ "Let's go"
            case playerOrTeam of
            PlayerOrTeamInput.Player -> SetStep Player
            PlayerOrTeamInput.Team -> SetStep Team
        ]
    ]
renderPage { step: Player, player, game } =
    [ boardingStep
        [ boardingHeading "Player details"
        , boardingDescription  """Enter details about yourself so your new bruh gamer friends
                can find you, bruh. Fill out as much as you can to ensure the
                bruhest gamers find you. All fields are optional, bruh."""
        , PlayerFormInput.playerFormInput player (Just <<< UpdatePlayer)
        ]
    , boardingButtons
        [ secondaryButton_ "Back" $ SetStep Greeting
        , primaryButton_ "Next" $ SetStep
            case game of
            Preselected _ -> PlayerProfile
            _ -> Game
        ]
    ]
renderPage { step: Team, team, game } =
    [ boardingStep
        [ boardingHeading "Team details"
        , boardingDescription  """Enter details about yourself so your new bruh gamer friends
                can find you, bruh. Fill out as much as you can to ensure the
                bruhest gamers find you. All fields are optional, bruh."""
        , TeamFormInput.teamFormInput team (Just <<< UpdateTeam)
        ]
    , boardingButtons
        [ secondaryButton_ "Back" $ SetStep Greeting
        , primaryButton_ "Next" $ SetStep
            case game of
            Preselected _ -> TeamProfile
            _ -> Game
        ]
    ]
    [ boardingStep
        [ boardingHeading "Game"
        , boardingDescription  """Select a game to create your first profile muhfugga."""
        , gameInput (getGame game) (Just <<< UpdateGame)
        ]
    , boardingButtons
        [ secondaryButton_ "Back"
            case playerOrTeam of
            PlayerOrTeamInput.Player -> SetStep Player
            PlayerOrTeamInput.Team -> SetStep Team
        , HH.button
            [ HP.class_ $ HH.ClassName "primary-button"
            , HP.disabled $ isNothing (getGame game)
            , HE.onClick $ const $ Just $ SetStep
                case playerOrTeam of
                PlayerOrTeamInput.Player -> PlayerProfile
                PlayerOrTeamInput.Team -> TeamProfile
            ]
            [ HH.text "Next" ]
        ]
    ]
renderPage { step: PlayerProfile, playerProfile, otherError, submitting, game } =
    [ boardingStep
        [ boardingHeading "Player profile details"
        , boardingDescription  """Enter details about your gameplay. Fill out everything."""
        , PlayerProfileFormInput.profileFormInput playerProfile UpdatePlayerProfile
        ]
    , boardingButtons
        [ secondaryButton_ "Back" $ SetStep
            case game of
            Preselected _ -> Player
            _ -> Game
        , primaryButton_ "Next" $ SetStep Register
        ]
    ]
renderPage { step: TeamProfile, teamProfile, otherError, submitting, game } =
    [ boardingStep
        [ boardingHeading "Team profile details"
        , boardingDescription  """Enter details about your gameplay. Fill out everything."""
        , TeamProfileFormInput.profileFormInput teamProfile UpdateTeamProfile
        ]
    , boardingButtons
        [ secondaryButton_ "Back" $ SetStep
            case game of
            Preselected _ -> Team
            _ -> Game
        , primaryButton_ "Next" $ SetStep Register
        ]
    ]
renderPage { step: Register, registration, otherError, submitting, playerOrTeam } =
    [ boardingStep
        [ boardingHeading "Registration details"
        , boardingDescription  """Enter your nickname, email address and password to complete the registration process."""
        , registrationInput registration UpdateRegistration
        ]
    , boardingButtons
        [ secondaryButton_ "Back" $ SetStep
            case playerOrTeam of
            PlayerOrTeamInput.Player -> PlayerProfile
            PlayerOrTeamInput.Team -> TeamProfile
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
    State -> Async left (Maybe (Either Preboarding.BadContent (Maybe Preboarding.OkContent)))
sendRequest (state :: State) = Async.unify do
    (body :: Preboarding.RequestContent) <-
        case state of
        { playerOrTeam: PlayerOrTeamInput.Player
        , player
        , playerProfile: profile
        , registration
        } | Just game <- getGame state.game -> Async.right
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
            , registration:
                { nickname: registration.nickname
                , email: registration.email
                , password: registration.password
                }
            }
        { playerOrTeam: PlayerOrTeamInput.Team
        , team
        , teamProfile: profile
        , registration
        } | Just game <- getGame state.game -> Async.right
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
            , registration:
                { nickname: registration.nickname
                , email: registration.email
                , password: registration.password
                }
            }
        _ -> Async.left Nothing
    response <-
        Fetch.fetch "/api/preboarding"
        ( Fetch.method := POST
        <> Fetch.body := Json.writeJSON body
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const Nothing)
    content :: Either Preboarding.BadContent $ Maybe Preboarding.OkContent <-
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
        Greeting -> navigateReplace state "/preboarding/start"
        Player -> navigateReplace state "/preboarding/player"
        Team -> navigateReplace state "/preboarding/team"
        Game -> navigateReplace state "/preboarding/game"
        PlayerProfile -> navigateReplace state "/preboarding/player-profile"
        TeamProfile -> navigateReplace state "/preboarding/team-profile"
        Register -> navigateReplace state "/preboarding/register"

handleAction :: forall action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction Initialize =
    H.liftEffect do
        setMetaTitle "Preboarding | TeamTavern"
        setMetaDescription "TeamTavern preboarding."
        setMetaUrl
handleAction (Receive input) =
    H.put (input # Record.insert (SProxy :: SProxy "submitting") false)
handleAction Exit = do
    { game } <- H.get
    navigate_
        case game of
        Selected _ -> "/"
        Preselected { handle } -> "/games/" <> handle
handleAction (SetStep step) = do
    state <- H.get
    case step of
        Greeting -> navigate state "/preboarding/start"
        Player -> navigate state "/preboarding/player"
        Team -> navigate state "/preboarding/team"
        Game -> navigate state "/preboarding/game"
        PlayerProfile -> navigate state "/preboarding/player-profile"
        TeamProfile -> navigate state "/preboarding/team-profile"
        Register -> navigate state "/preboarding/register"
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
        { game = Selected $ Just game
        , playerProfile
            { fields = game.fields
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
            { fieldValues = details.fieldValues
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
handleAction (UpdateRegistration registration) = do
    state <- H.modify _
        { registration
            { nickname = registration.nickname
            , email = registration.email
            , password = registration.password
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
                { urlErrors = []
                , missingErrors = []
                , ambitionsError = false
                }
            }
    response <- H.lift $ sendRequest currentState
    case response of
        Just (Right Nothing) -> navigate_ "/"
        Just (Right (Just input)) -> navigate input $ "/welcome"
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
                        { url: \{ key } -> state'
                            { step =
                                if state'.step > PlayerProfile then PlayerProfile else state'.step
                            , playerProfile
                                { urlErrors = Array.cons key state'.playerProfile.urlErrors }
                            }
                        , missing: \{ key } -> state'
                            { step =
                                if state'.step > PlayerProfile then PlayerProfile else state'.step
                            , playerProfile
                                { missingErrors =
                                    Array.cons key state'.playerProfile.missingErrors
                                }
                            }
                        , ambitions: const $ state'
                            { step =
                                if state'.step > PlayerProfile then PlayerProfile else state'.step
                            , playerProfile { ambitionsError = true }
                            }
                        }
                        error'
                    )
                    state
                , teamProfile:
                    foldl
                    (\state' error' ->
                        match
                        { ambitions: const $ state'
                            { step =
                                if state'.step > TeamProfile then TeamProfile else state'.step
                            , playerProfile { ambitionsError = true } }
                            }
                        error'
                    )
                    state
                , registration:
                    foldl
                    (\state' error' ->
                        match
                        { nickname: const $ state' { registration { nicknameError = true } }
                        , email: const $ state' { registration { emailError = true } }
                        , password: const $ state' { registration { passwordError = true } }
                        }
                        error'
                    )
                    state
                , nicknameTaken: const $ state { registration { nicknameTaken = true } }
                , emailTaken: const $ state { registration { emailTaken = true } }
                }
                error
            )
            (nextState { otherError = false })
            errors
        Nothing -> H.put nextState { otherError = true }

component :: forall query output left.
    H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: Record.insert (SProxy :: SProxy "submitting") false
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

preboarding :: forall action slots left.
    Input -> HH.ComponentHTML action (preboarding :: Slot | slots) (Async left)
preboarding input = HH.slot (SProxy :: SProxy "preboarding") unit component input absurd

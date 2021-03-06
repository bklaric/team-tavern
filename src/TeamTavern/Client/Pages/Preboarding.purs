module TeamTavern.Client.Pages.Preboarding (PlayerOrTeam(..), Game(..), Step(..), Input, Slot, emptyInput, preboarding) where

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
import Record.Extra (pick)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.Boarding.Boarding (boarding, boardingButtons, boardingDescription, boardingHeading, boardingStep)
import TeamTavern.Client.Components.Boarding.GameInput (gameInput)
import TeamTavern.Client.Components.Boarding.GameInput as GameInput
import TeamTavern.Client.Components.Boarding.PlayerOrTeamInput (playerOrTeamInput)
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
import TeamTavern.Routes.Preboard as Preboard
import TeamTavern.Routes.Shared.Platform (Platform(..))
import TeamTavern.Routes.ViewGame as ViewGame

data Step
    = Greeting
    | PlayerOrTeam
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
    writeImpl PlayerOrTeam = unsafeToForeign "PlayerOrTeam"
    writeImpl Player = unsafeToForeign "Player"
    writeImpl Team = unsafeToForeign "Team"
    writeImpl Game = unsafeToForeign "Game"
    writeImpl PlayerProfile = unsafeToForeign "PlayerProfile"
    writeImpl TeamProfile = unsafeToForeign "TeamProfile"
    writeImpl Register = unsafeToForeign "Register"

instance readForeignStep :: ReadForeign Step where
    readImpl = readString >=> case _ of
        "Greeting" -> pure Greeting
        "PlayerOrTeam" -> pure PlayerOrTeam
        "Player" -> pure Player
        "Team" -> pure Team
        "Game" -> pure Game
        "PlayerProfile" -> pure PlayerProfile
        "TeamProfile" -> pure TeamProfile
        "Register" -> pure Register
        step -> fail $ ForeignError $ "Unknown step " <> step

data PlayerOrTeam
    = Preselected' PlayerOrTeamInput.PlayerOrTeam
    | Selected' (Maybe PlayerOrTeamInput.PlayerOrTeam)

getPlayerOrTeam :: PlayerOrTeam -> Maybe PlayerOrTeamInput.PlayerOrTeam
getPlayerOrTeam (Preselected' playerOrTeam) = Just playerOrTeam
getPlayerOrTeam (Selected' (Just playerOrTeam)) = Just playerOrTeam
getPlayerOrTeam _ = Nothing

instance writeForeginPlayerOrTeam :: WriteForeign PlayerOrTeam where
    writeImpl (Preselected' playerOrTeam) = writeImpl { preselected: playerOrTeam }
    writeImpl (Selected' input) = writeImpl { selected: input }

instance readForeignPlayerOrTeam :: ReadForeign PlayerOrTeam where
    readImpl foreign' =
        ( (readImpl foreign' :: _ { preselected :: PlayerOrTeamInput.PlayerOrTeam })
            <#> _.preselected <#> Preselected'
        )
        <|>
        ( (readImpl foreign' :: _ { selected :: Maybe PlayerOrTeamInput.PlayerOrTeam })
            <#> _.selected <#> Selected'
        )

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
    , playerOrTeam :: PlayerOrTeam
    , player :: PlayerFormInput.Input
    , team :: TeamFormInput.Input
    , game :: Game
    , playerProfile :: PlayerProfileFormInput.Input
    , teamProfile :: TeamProfileFormInput.Input
    , registration :: RegistrationInput.Input
    , otherError :: Boolean
    }

emptyInput :: Maybe PlayerOrTeamInput.PlayerOrTeam -> Maybe ViewGame.OkContent -> Input
emptyInput playerOrTeam game =
    { step: Greeting
    , playerOrTeam:
        case playerOrTeam of
        Just playerOrTeam' -> Preselected' playerOrTeam'
        Nothing -> Selected' Nothing
    , player: PlayerFormInput.emptyInput
    , team: TeamFormInput.emptyInput
    , game:
        case game of
        Just game' -> Preselected game'
        Nothing -> Selected Nothing
    , playerProfile: PlayerProfileFormInput.emptyInput $
        maybe { platforms: { head: Steam, tail: [] }, fields: [] } pick game
    , teamProfile: TeamProfileFormInput.emptyInput $
        maybe { platforms: { head: Steam, tail: [] }, fields: [] } pick game
        # \game' -> game'
            { fields = game'.fields # mapMaybe
                case _ of
                { ilk, key, label, icon, options: Just options } | ilk == 2 || ilk == 3 ->
                    Just { key, label, icon, options: options }
                _ -> Nothing
            }
    , registration: RegistrationInput.emptyInput
    , otherError: false
    }

type State =
    { step :: Step
    , playerOrTeam :: PlayerOrTeam
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
    | UpdatePlayerOrTeam PlayerOrTeamInput.PlayerOrTeam
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
        , primaryButton_ "Let's go" $ SetStep
            case playerOrTeam of
            Preselected' (PlayerOrTeamInput.Player) -> Player
            Preselected' (PlayerOrTeamInput.Team) -> Team
            Selected' _ -> PlayerOrTeam
        ]
    ]
renderPage { step: PlayerOrTeam, playerOrTeam } =
    [ boardingStep
        [ boardingHeading "Player or team"
        , boardingDescription "Do you want to create your own player profile or a team profile?"
        , playerOrTeamInput (getPlayerOrTeam playerOrTeam) UpdatePlayerOrTeam
        ]
    , boardingButtons
        [ secondaryButton_ "Back" $ SetStep Greeting
        , HH.button
            [ HP.class_ $ HH.ClassName "primary-button"
            , HP.disabled $ isNothing (getPlayerOrTeam playerOrTeam)
            , HE.onClick $ const $ Just $ SetStep
                case playerOrTeam of
                Preselected' (PlayerOrTeamInput.Player) -> Player
                Preselected' (PlayerOrTeamInput.Team) -> Team
                Selected' (Just PlayerOrTeamInput.Player) -> Player
                Selected' (Just PlayerOrTeamInput.Team) -> Team
                Selected' Nothing -> PlayerOrTeam
            ]
            [ HH.text "Next" ]
        ]
    ]
renderPage { step: Player, player, playerOrTeam, game } =
    [ boardingStep
        [ boardingHeading "Player"
        , boardingDescription  """Tell us about yourself. Fill out as much as you want to help us
            find you the right teammates. All fields are optional."""
        , PlayerFormInput.playerFormInput player (Just <<< UpdatePlayer)
        ]
    , boardingButtons
        [ secondaryButton_ "Back" $ SetStep
            case playerOrTeam of
            Preselected' _ -> Greeting
            _ -> PlayerOrTeam
        , primaryButton_ "Next" $ SetStep
            case game of
            Preselected _ -> PlayerProfile
            _ -> Game
        ]
    ]
renderPage { step: Team, team, playerOrTeam, game } =
    [ boardingStep
        [ boardingHeading "Team"
        , boardingDescription  """Tell us about your team. Fill out as much as you want to help us
            find the right teammates for your team. All fields are optional."""
        , TeamFormInput.teamFormInput team (Just <<< UpdateTeam)
        ]
    , boardingButtons
        [ secondaryButton_ "Back" $ SetStep
            case playerOrTeam of
            Preselected' _ -> Greeting
            _ -> PlayerOrTeam
        , primaryButton_ "Next" $ SetStep
            case game of
            Preselected _ -> TeamProfile
            _ -> Game
        ]
    ]
renderPage { step: Game, game, playerOrTeam } =
    [ boardingStep
        [ boardingHeading "Game"
        , boardingDescription  """Select a game to create your first profile."""
        , gameInput (getGame game) (Just <<< UpdateGame)
        ]
    , boardingButtons
        [ secondaryButton_ "Back" $ SetStep
            case playerOrTeam of
            Preselected' (PlayerOrTeamInput.Player) -> Player
            Preselected' (PlayerOrTeamInput.Team) -> Team
            Selected' (Just PlayerOrTeamInput.Player) -> Player
            Selected' (Just PlayerOrTeamInput.Team) -> Team
            Selected' Nothing -> PlayerOrTeam
        , HH.button
            [ HP.class_ $ HH.ClassName "primary-button"
            , HP.disabled $ isNothing (getGame game)
            , HE.onClick $ const $ Just $ SetStep
                case playerOrTeam of
                Preselected' (PlayerOrTeamInput.Player) -> PlayerProfile
                Preselected' (PlayerOrTeamInput.Team) -> TeamProfile
                Selected' (Just PlayerOrTeamInput.Player) -> PlayerProfile
                Selected' (Just PlayerOrTeamInput.Team) -> TeamProfile
                Selected' Nothing -> PlayerOrTeam
            ]
            [ HH.text "Next" ]
        ]
    ]
renderPage { step: PlayerProfile, playerProfile, otherError, submitting, game } =
    [ boardingStep
        [ boardingHeading $ maybe "Player profile" (\{ title } -> title <> " player profile") (getGame game)
        , boardingDescription  """Fill out your in-game stats, achievements and ambitions to find
            equally skilled teammates."""
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
        [ boardingHeading $ maybe "Team profile" (\{ title } -> title <> " team profile") (getGame game)
        , boardingDescription  """Tell us about your team's ambitions and what you're looking for
            skill-wise in new team members."""
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
        [ boardingHeading "Registration"
        , boardingDescription  """Enter your nickname and password to complete the registration process."""
        , registrationInput registration UpdateRegistration
        ]
    , boardingButtons
        [ secondaryButton_ "Back" $ SetStep
            case playerOrTeam of
            Preselected' (PlayerOrTeamInput.Player) -> PlayerProfile
            Preselected' (PlayerOrTeamInput.Team) -> TeamProfile
            Selected' (Just PlayerOrTeamInput.Player) -> PlayerProfile
            Selected' (Just PlayerOrTeamInput.Team) -> TeamProfile
            Selected' Nothing -> PlayerOrTeam
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
    State -> Async left (Maybe (Either Preboard.BadContent Preboard.OkContent))
sendRequest (state :: State) = Async.unify do
    (body :: Preboard.RequestContent) <-
        case state of
        { player
        , playerProfile: profile
        , registration
        } | Just game <- getGame state.game
          , Just (PlayerOrTeamInput.Player) <- getPlayerOrTeam state.playerOrTeam -> Async.right
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
                { platform: profile.platform
                , platformId: profile.platformId
                , fieldValues: profile.fieldValues
                , newOrReturning: profile.newOrReturning
                , ambitions: profile.ambitions
                }
            , teamProfile: Nothing
            , registration:
                { nickname: registration.nickname
                , password: registration.password
                }
            }
        { team
        , teamProfile: profile
        , registration
        } | Just game <- getGame state.game
          , Just (PlayerOrTeamInput.Team) <- getPlayerOrTeam state.playerOrTeam -> Async.right
            { ilk: 2
            , player: Nothing
            , team: Just
                { name: team.name
                , website: team.website
                , discordTag: team.discordTag
                , discordServer: team.discordServer
                , ageFrom: team.ageFrom
                , ageTo: team.ageTo
                , locations: team.locations
                , languages: team.languages
                , microphone: team.microphone
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
                { platforms: profile.selectedPlatforms
                , fieldValues: profile.fieldValues
                , newOrReturning: profile.newOrReturning
                , ambitions: profile.ambitions
                }
            , registration:
                { nickname: registration.nickname
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
    content :: Either Preboard.BadContent Preboard.OkContent <-
        case FetchRes.status response of
        200 -> FetchRes.text response >>= JsonAsync.readJSON # bimap (const Nothing) Right
        400 -> FetchRes.text response >>= JsonAsync.readJSON # bimap (const Nothing) Left
        _ -> Async.left Nothing
    pure $ Just content

-- Update state for current history entry so back button doesn't lose previous state.
updateHistoryState :: forall monad. MonadEffect monad => State -> monad Unit
updateHistoryState (state :: State) = do
    case state.step of
        Greeting -> navigateReplace state "/preboarding/start"
        PlayerOrTeam -> navigateReplace state "/preboarding/player-or-team"
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
        PlayerOrTeam -> navigate state "/preboarding/player-or-team"
        Player -> navigate state "/preboarding/player"
        Team -> navigate state "/preboarding/team"
        Game -> navigate state "/preboarding/game"
        PlayerProfile -> navigate state "/preboarding/player-profile"
        TeamProfile -> navigate state "/preboarding/team-profile"
        Register -> navigate state "/preboarding/register"
handleAction (UpdatePlayerOrTeam playerOrTeam) = do
    state <- H.modify _ { playerOrTeam = Selected' $ Just playerOrTeam }
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
            , discordTag = details.discordTag
            , discordServer = details.discordServer
            , ageFrom = details.ageFrom
            , ageTo = details.ageTo
            , locations = details.locations
            , languages = details.languages
            , microphone = details.microphone
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
            { platforms = game.platforms
            , fields = game.fields
            , platform = game.platforms.head
            , fieldValues = []
            , newOrReturning = false
            , ambitions = ""
            }
        , teamProfile
            { allPlatforms = game.platforms
            , selectedPlatforms = [ game.platforms.head ]
            , fields = game.fields # Array.mapMaybe
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
            { platform = details.platform
            , platformId = details.platformId
            , platformIdError = details.platformIdError
            , fieldValues = details.fieldValues
            , newOrReturning = details.newOrReturning
            , ambitions = details.ambitions
            }
        }
    updateHistoryState state
handleAction (UpdateTeamProfile details) = do
    state <- H.modify _
        { teamProfile
            { selectedPlatforms = details.platforms
            , fieldValues = details.fieldValues
            , newOrReturning = details.newOrReturning
            , ambitions = details.ambitions
            }
        }
    updateHistoryState state
handleAction (UpdateRegistration registration) = do
    state <- H.modify _
        { registration
            { nickname = registration.nickname
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
                , discordTagError = false
                , discordServerError = false
                , contactError = false
                , aboutError = false
                }
            , playerProfile
                { platformIdError = false
                , urlErrors = []
                , ambitionsError = false
                }
            , teamProfile
                { platformsError = false
                , ambitionsError = false
                }
            , registration
                { nicknameError = false
                , passwordError = false
                , nicknameTaken = false
                }
            }
    response <- H.lift $ sendRequest currentState
    case response of
        Just (Right { teamHandle: Nothing }) -> navigate_ "/"
        Just (Right { teamHandle: Just teamHandle }) -> navigate_ $ "/teams/" <> teamHandle
        Just (Left errors) -> H.put $
            foldl
            (\state error ->
                match
                { player:
                    foldl
                    (\state' error' ->
                        match
                        { discordTag: const state'
                            { step = Player, player { discordTagError = true } }
                        , about: const state'
                            { step = Player, player { aboutError = true } }
                        }
                        error'
                    )
                    state
                , team:
                    foldl
                    (\state' error' ->
                        match
                        { name: const state'
                            { step = Team, team { nameError = true } }
                        , website: const state'
                            { step = Team, team { websiteError = true } }
                        , discordTag: const state'
                            { step = Team, team { discordTagError = true } }
                        , discordServer: const state'
                            { step = Team, team { discordServerError = true } }
                        , contact: const state'
                            { step = Team, team { contactError = true } }
                        , about: const state'
                            { step = Team, team { aboutError = true } }
                        }
                        error'
                    )
                    state
                , playerProfile:
                    foldl
                    (\state' error' ->
                        match
                        { platformId: const state'
                            { step =
                                if state'.step > PlayerProfile then PlayerProfile else state'.step
                            , playerProfile { platformIdError = true }
                            }
                        , url: \{ key } -> state'
                            { step =
                                if state'.step > PlayerProfile then PlayerProfile else state'.step
                            , playerProfile
                                { urlErrors = Array.cons key state'.playerProfile.urlErrors }
                            }
                        , ambitions: const state'
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
                        { platforms: const state'
                            { step = if state'.step > TeamProfile then TeamProfile else state'.step
                            , teamProfile { platformsError = true }
                            }
                        , ambitions: const state'
                            { step = if state'.step > TeamProfile then TeamProfile else state'.step
                            , teamProfile { ambitionsError = true }
                            }
                        }
                        error'
                    )
                    state
                , registration:
                    foldl
                    (\state' error' ->
                        match
                        { nickname: const state' { registration { nicknameError = true } }
                        , password: const state' { registration { passwordError = true } }
                        }
                        error'
                    )
                    state
                , nicknameTaken: const state { registration { nicknameTaken = true } }
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

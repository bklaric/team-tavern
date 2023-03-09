module TeamTavern.Client.Pages.Onboarding (Step(..), Input, emptyInput, onboarding) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Monoid (guard)
import Data.Variant (match, onMatch)
import Effect.Class (class MonadEffect)
import Foreign (ForeignError(..), fail, readString, unsafeToForeign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record
import Record.Extra (pick)
import TeamTavern.Client.Components.Boarding.Boarding (boarding, boardingButtons, boardingDescription, boardingHeading, boardingStep)
import TeamTavern.Client.Components.Boarding.GameInput (gameInput)
import TeamTavern.Client.Components.Boarding.GameInput as GameInput
import TeamTavern.Client.Components.Boarding.PlayerOrTeamInput (playerOrTeamInput)
import TeamTavern.Client.Components.Boarding.PlayerOrTeamInput as PlayerOrTeamInput
import TeamTavern.Client.Components.Button (primaryButton_, secondaryButton_)
import TeamTavern.Client.Components.Player.PlayerFormInput as PlayerFormInput
import TeamTavern.Client.Components.Player.ProfileFormInput as PlayerProfileFormInput
import TeamTavern.Client.Components.Team.ProfileFormInput as TeamProfileFormInput
import TeamTavern.Client.Components.Team.TeamFormInput as TeamFormInput
import TeamTavern.Client.Script.Analytics (track)
import TeamTavern.Client.Script.Cookie (getPlayerNickname)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Navigate (navigate, navigate_, replaceState)
import TeamTavern.Client.Shared.Fetch (fetchBody)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Boarding.Onboard (Onboard)
import TeamTavern.Routes.Boarding.Onboard as Onboard
import TeamTavern.Routes.Shared.Platform (Platform(..))
import Type.Proxy (Proxy(..))
import Yoga.JSON (class ReadForeign, class WriteForeign)

data Step
    = Greeting
    | PlayerOrTeam
    | Player
    | Team
    | Game
    | PlayerProfile
    | TeamProfile

derive instance Eq Step

instance WriteForeign Step where
    writeImpl Greeting = unsafeToForeign "Greeting"
    writeImpl PlayerOrTeam = unsafeToForeign "PlayerOrTeam"
    writeImpl Player = unsafeToForeign "Player"
    writeImpl Team = unsafeToForeign "Team"
    writeImpl Game = unsafeToForeign "Game"
    writeImpl PlayerProfile = unsafeToForeign "PlayerProfile"
    writeImpl TeamProfile = unsafeToForeign "TeamProfile"

instance ReadForeign Step where
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
    , playerProfile: PlayerProfileFormInput.emptyInput
        { platforms: { head: Steam, tail: [] }, fields: [] }
    , teamProfile: TeamProfileFormInput.emptyInput
        { platforms: { head: Steam, tail: [] }, fields: [] }
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

type ChildSlots slots =
    ( playerFormInput :: PlayerFormInput.Slot
    , teamFormInput :: TeamFormInput.Slot
    , gameInput :: GameInput.Slot
    , playerProfileFormInput :: PlayerProfileFormInput.Slot
    , teamProfileFormInput :: TeamProfileFormInput.Slot
    | slots )

renderPage :: ∀ slots left.
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
                Just PlayerOrTeamInput.Player -> SetStep Player
                Just PlayerOrTeamInput.Team -> SetStep Team
                Nothing -> SetStep Player
            ]
            [ HH.text "Next" ]
        ]
    ]
renderPage { step: Player, player } =
    [ boardingStep
        [ boardingHeading "Player"
        , boardingDescription  """Tell us about yourself. Fill out as much as you want to help us
            find you the right teammates. All fields are optional."""
        , PlayerFormInput.playerFormInput player UpdatePlayer
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
        , TeamFormInput.teamFormInput team UpdateTeam
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
        , gameInput game UpdateGame
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
            , HE.onClick $ const $ SetStep
                case playerOrTeam of
                Just PlayerOrTeamInput.Player -> PlayerProfile
                Just PlayerOrTeamInput.Team -> TeamProfile
                Nothing -> PlayerProfile
            ]
            [ HH.text "Next" ]
        ]
    ]
renderPage { step: PlayerProfile, playerProfile, game, otherError, submitting } =
    [ boardingStep
        [ boardingHeading $ maybe "Player profile" (\{ title } -> title <> " player profile") game
        , boardingDescription  """Fill out your in-game stats, achievements and about to find
            equally skilled teammates."""
        , PlayerProfileFormInput.profileFormInput playerProfile UpdatePlayerProfile
        ]
    , boardingButtons
        [ secondaryButton_ "Back" $ SetStep Game
        , HH.div [ HS.class_ "boarding-submit-button-group" ] $
            [ HH.button
                [ HS.class_ "primary-button"
                , HP.disabled submitting
                , HE.onClick $ const SetUpAccount
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
renderPage { step: TeamProfile, teamProfile, game, otherError, submitting } =
    [ boardingStep
        [ boardingHeading $ maybe "Team profile" (\{ title } -> title <> " team profile") game
        , boardingDescription  """Tell us about your team's about and what you're looking for
            skill-wise in new team members."""
        , TeamProfileFormInput.profileFormInput teamProfile UpdateTeamProfile
        ]
    , boardingButtons
        [ secondaryButton_ "Back" $ SetStep Game
        , HH.div [ HS.class_ "boarding-submit-button-group" ] $
            [ HH.button
                [ HS.class_ "primary-button"
                , HP.disabled submitting
                , HE.onClick $ const SetUpAccount
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

render :: ∀ slots left. State -> HH.ComponentHTML Action (ChildSlots slots) (Async left)
render state = HH.div_ [boarding $ renderPage state]

sendRequest :: ∀ left.
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
            , player: Just $ pick player
            , team: Nothing
            , gameHandle: game.handle
            , playerProfile: Just $ pick profile.details
            , teamProfile: Nothing
            , playerContacts: Just $ pick profile.contacts
            , teamContacts: Nothing
            }
        { playerOrTeam: Just PlayerOrTeamInput.Team
        , team: team
        , game: Just game
        , teamProfile: profile
        } -> Async.right
            { ilk: 2
            , player: Nothing
            , team: Just $ pick team
            , gameHandle: game.handle
            , playerProfile: Nothing
            , teamProfile: Just $ pick $ Record.insert
                (Proxy :: _ "platforms") profile.details.selectedPlatforms profile.details
            , playerContacts: Nothing
            , teamContacts: Just $ pick profile.contacts
            }
        _ -> Async.left Nothing
    response <- fetchBody (Proxy :: _ Onboard) body # lmap (const Nothing)
    let result = onMatch
            { ok: Just <<< Right
            , badRequest: Just <<< Left
            }
            (const Nothing)
            response
    let trackParams =
            { ilk: if body.ilk == 1 then "player" else "team"
            , game: body.gameHandle
            , result: show result
            }
    case result of
        Just (Right _) -> track "Onboard" trackParams
        _ -> track "Onboard error" trackParams
    pure result

-- Update state for current history entry so back button doesn't lose previous state.
updateHistoryState :: ∀ monad. MonadEffect monad => State -> monad Unit
updateHistoryState (state :: State) = do
    case state.step of
        Greeting -> replaceState state "/onboarding/start"
        PlayerOrTeam -> replaceState state "/onboarding/player-or-team"
        Player -> replaceState state "/onboarding/player"
        Team -> replaceState state "/onboarding/team"
        Game -> replaceState state "/onboarding/game"
        PlayerProfile -> replaceState state "/onboarding/player-profile"
        TeamProfile -> replaceState state "/onboarding/team-profile"

handleAction :: ∀ action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction Initialize = do
    nickname <- getPlayerNickname
    case nickname of
        Just nickname' -> H.modify_ _ { nickname = nickname' }
        Nothing -> navigate_ "/"
    setMeta "Onboarding | TeamTavern" "TeamTavern onboarding."
handleAction (Receive input) = do
    state <- H.get
    H.put
        ( input
        # Record.insert (Proxy :: _ "confirmSkip") false
        # Record.insert (Proxy :: _ "submitting") false
        )
    guard (input.step /= state.step) $ setMeta "Onboarding | TeamTavern" "TeamTavern onboarding."
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
    state <- H.modify \state -> state { player = Record.merge details state.player }
    updateHistoryState state
handleAction (UpdateTeam details) = do
    state <- H.modify \state -> state { team = Record.merge details state.team }
    updateHistoryState state
handleAction (UpdateGame game) = do
    state <- H.modify _
        { game = Just game
        , playerProfile
            { details
                { platforms = game.platforms
                , fields = game.fields
                , platform = game.platforms.head
                , fieldValues = []
                , newOrReturning = false
                , about = ""
                }
            }
        , teamProfile
            { details
                { allPlatforms = game.platforms
                , selectedPlatforms = [ game.platforms.head ]
                , fields = game.fields # Array.mapMaybe
                    case _ of
                    { ilk, key, label, icon, options: Just options } | ilk == 2 || ilk == 3 ->
                        Just { key, label, icon, options }
                    _ -> Nothing
                , fieldValues = []
                , newOrReturning = false
                , about = ""
                }
            }
        }
    updateHistoryState state
handleAction (UpdatePlayerProfile profile) = do
    state <- H.modify \state -> state
        { playerProfile
            { details = Record.merge profile.details state.playerProfile.details
            , contacts = Record.merge profile.contacts state.playerProfile.contacts
            }
        }
    updateHistoryState state
handleAction (UpdateTeamProfile profile) = do
    state <- H.modify \state -> state
        { teamProfile
            { details = Record.merge profile.details state.teamProfile.details
            , contacts = Record.merge profile.contacts state.teamProfile.contacts
            }
        }
    updateHistoryState state
handleAction SetUpAccount = do
    currentState <- H.modify _ { submitting = true }
    let nextState = currentState
            { submitting = false
            , team
                { nameError = false
                , websiteError = false
                }
            , playerProfile
                { details
                    { urlErrors = []
                    , aboutError = false
                    , ambitionsError = false
                    }
                , contacts
                    { discordTagError = false
                    , steamIdError = false
                    , riotIdError = false
                    , battleTagError = false
                    , eaIdError = false
                    , ubisoftUsernameError = false
                    , psnIdError = false
                    , gamerTagError = false
                    , friendCodeError = false
                    }
                }
            , teamProfile
                { details
                    { platformsError = false
                    , aboutError = false
                    , ambitionsError = false
                    }
                , contacts
                    { discordTagError = false
                    , steamIdError = false
                    , riotIdError = false
                    , battleTagError = false
                    , eaIdError = false
                    , ubisoftUsernameError = false
                    , psnIdError = false
                    , gamerTagError = false
                    , friendCodeError = false
                    }
                }
            }
    response <- H.lift $ sendRequest currentState
    case response of
        Just (Right {teamHandle: Nothing}) -> navigate_ "/"
        Just (Right {teamHandle: Just teamHandle}) -> navigate_ $ "/teams/" <> teamHandle
        Just (Left errors) -> H.put $
            foldl
            (\state error ->
                match
                { team: state # foldl \state' error' -> error' # match
                    { name: const state' { step = Team, team { nameError = true } }
                    , website: const state' { step = Team, team { websiteError = true } }
                    }
                , playerProfile: state # foldl \state' error' -> error' # match
                    { url: \{ key } -> state'
                        { playerProfile { details { urlErrors = Array.cons key state'.playerProfile.details.urlErrors } } }
                    , about: const state'
                        { playerProfile { details { aboutError = true } } }
                    , ambitions: const state'
                        { playerProfile { details { ambitionsError = true } } }
                    }
                , teamProfile: state # foldl \state' error' -> error' # match
                    { platforms: const state' { teamProfile { details { platformsError = true } } }
                    , about: const state' { teamProfile { details { aboutError = true } } }
                    , ambitions: const state' { teamProfile { details { ambitionsError = true } } }
                    }
                , playerContacts: state # foldl \state' error' -> error' # match
                    { discordTag: const state' { playerProfile { contacts { discordTagError = true } } }
                    , steamId: const state' { playerProfile { contacts { steamIdError = true } } }
                    , riotId: const state' { playerProfile { contacts { riotIdError = true } } }
                    , battleTag: const state' { playerProfile { contacts { battleTagError = true } } }
                    , eaId: const state' { playerProfile { contacts { eaIdError = true } } }
                    , ubisoftUsername: const state' { playerProfile { contacts { ubisoftUsernameError = true } } }
                    , psnId: const state' { playerProfile { contacts { psnIdError = true } } }
                    , gamerTag: const state' { playerProfile { contacts { gamerTagError = true } } }
                    , friendCode: const state' { playerProfile { contacts { friendCodeError = true } } }
                    }
                , teamContacts: state # foldl \state' error' -> error' # match
                    { discordTag: const state' { teamProfile { contacts { discordTagError = true } } }
                    , discordServer: const state' { teamProfile { contacts { discordServerError = true } } }
                    , steamId: const state' { teamProfile { contacts { steamIdError = true } } }
                    , riotId: const state' { teamProfile { contacts { riotIdError = true } } }
                    , battleTag: const state' { teamProfile { contacts { battleTagError = true } } }
                    , eaId: const state' { teamProfile { contacts { eaIdError = true } } }
                    , ubisoftUsername: const state' { teamProfile { contacts { ubisoftUsernameError = true } } }
                    , psnId: const state' { teamProfile { contacts { psnIdError = true } } }
                    , gamerTag: const state' { teamProfile { contacts { gamerTagError = true } } }
                    , friendCode: const state' { teamProfile { contacts { friendCodeError = true } } }
                    }
                , other: const state { otherError = true }
                }
                error
            )
            (nextState { otherError = false })
            errors
        Nothing -> H.put nextState { otherError = true }

component :: ∀ query output left.
    H.Component query Input output (Async left)
component = H.mkComponent
    { initialState:
        Record.insert (Proxy :: _ "confirmSkip") false
        >>> Record.insert (Proxy :: _ "submitting") false
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

onboarding :: ∀ action slots left.
    Input -> HH.ComponentHTML action (onboarding :: Slot___ | slots) (Async left)
onboarding input = HH.slot (Proxy :: _ "onboarding") unit component input absurd

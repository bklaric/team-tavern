module TeamTavern.Client.Pages.Preboarding (PlayerOrTeam(..), Game(..), Step(..), RegistrationMode(..), Input, emptyInput, preboarding) where

import Prelude

import Async (Async)
import Async as Async
import Control.Alt ((<|>))
import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Monoid (guard)
import Data.Variant (inj, match, onMatch)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (ForeignError(..), fail, readString, unsafeToForeign)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record
import Record.Extra (pick)
import TeamTavern.Client.Components.Ads (stickyLeaderboards)
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
import TeamTavern.Client.Components.RegistrationInputDiscord (registrationInputDiscord)
import TeamTavern.Client.Components.RegistrationInputDiscord as RegistrationInputDiscord
import TeamTavern.Client.Components.Team.ProfileFormInput as TeamProfileFormInput
import TeamTavern.Client.Components.Team.TeamFormInput as TeamFormInput
import TeamTavern.Client.Script.Analytics (aliasNickname, identifyNickname, track)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Navigate (hardNavigate, navigate, navigate_, replaceState)
import TeamTavern.Client.Shared.Fetch (fetchBody)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Boarding.Preboard (Preboard)
import TeamTavern.Routes.Boarding.Preboard as Preboard
import TeamTavern.Routes.Game.ViewGame as ViewGame
import TeamTavern.Routes.Shared.Platform (Platform(..))
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (setItem)
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl, writeJSON)

data Step
    = Greeting
    | PlayerOrTeam
    | Player
    | Team
    | Game
    | PlayerProfile
    | TeamProfile
    | Register

derive instance Eq Step

derive instance Ord Step

instance WriteForeign Step where
    writeImpl Greeting = unsafeToForeign "Greeting"
    writeImpl PlayerOrTeam = unsafeToForeign "PlayerOrTeam"
    writeImpl Player = unsafeToForeign "Player"
    writeImpl Team = unsafeToForeign "Team"
    writeImpl Game = unsafeToForeign "Game"
    writeImpl PlayerProfile = unsafeToForeign "PlayerProfile"
    writeImpl TeamProfile = unsafeToForeign "TeamProfile"
    writeImpl Register = unsafeToForeign "Register"

instance ReadForeign Step where
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

data RegistrationMode = Email | Discord

derive instance Eq RegistrationMode

instance WriteForeign RegistrationMode where
    writeImpl Email = unsafeToForeign "Email"
    writeImpl Discord = unsafeToForeign "Discord"

instance ReadForeign RegistrationMode where
    readImpl = readString >=> case _ of
        "Email" -> pure Email
        "Discord" -> pure Discord
        mode -> fail $ ForeignError $ "Unknown register mode " <> mode

data PlayerOrTeam
    = Preselected' PlayerOrTeamInput.PlayerOrTeam
    | Selected' (Maybe PlayerOrTeamInput.PlayerOrTeam)

getPlayerOrTeam :: PlayerOrTeam -> Maybe PlayerOrTeamInput.PlayerOrTeam
getPlayerOrTeam (Preselected' playerOrTeam) = Just playerOrTeam
getPlayerOrTeam (Selected' (Just playerOrTeam)) = Just playerOrTeam
getPlayerOrTeam _ = Nothing

instance WriteForeign PlayerOrTeam where
    writeImpl (Preselected' playerOrTeam) = writeImpl { preselected: playerOrTeam }
    writeImpl (Selected' input) = writeImpl { selected: input }

instance ReadForeign PlayerOrTeam where
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

instance WriteForeign Game where
    writeImpl (Preselected game) = writeImpl { preselected: game }
    writeImpl (Selected input) = writeImpl { selected: input }

instance ReadForeign Game where
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
    , registrationMode :: RegistrationMode
    , registrationEmail :: RegistrationInput.Input
    , registrationDiscord :: RegistrationInputDiscord.Input
    , accessToken :: Maybe String
    , discordError :: Boolean
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
    , registrationMode: Email
    , registrationEmail: RegistrationInput.emptyInput
    , registrationDiscord: RegistrationInputDiscord.emptyInput
    , accessToken: Nothing
    , discordError: false
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
    , registrationMode :: RegistrationMode
    , registrationEmail :: RegistrationInput.Input
    , registrationDiscord :: RegistrationInputDiscord.Input
    , accessToken :: Maybe String
    , discordError :: Boolean
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
    | UpdateRegistrationEmail RegistrationInput.Output
    | UpdateRegistrationDiscord RegistrationInputDiscord.Output
    | SetUpAccount
    | CreateWithEmail
    | CreateWithDiscord

type ChildSlots slots =
    ( playerFormInput :: PlayerFormInput.Slot
    , teamFormInput :: TeamFormInput.Slot
    , gameInput :: GameInput.Slot
    , playerProfileFormInput :: PlayerProfileFormInput.Slot
    , teamProfileFormInput :: TeamProfileFormInput.Slot
    , registrationInput :: RegistrationInput.Slot
    | slots )

renderPage :: ∀ slots left.
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
            , HE.onClick $ const $ SetStep
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
        , PlayerFormInput.playerFormInput player UpdatePlayer
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
            find the right teammates for your team."""
        , TeamFormInput.teamFormInput team UpdateTeam
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
        , gameInput (getGame game) UpdateGame
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
            , HE.onClick $ const $ SetStep
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
renderPage { step: PlayerProfile, playerProfile, game } =
    [ boardingStep
        [ boardingHeading $ maybe "Player profile" (\{ title } -> title <> " player profile") (getGame game)
        , boardingDescription  """Fill out your in-game stats, achievements and about to find
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
renderPage { step: TeamProfile, teamProfile, game } =
    [ boardingStep
        [ boardingHeading $ maybe "Team profile" (\{ title } -> title <> " team profile") (getGame game)
        , boardingDescription  """Tell us about your team's about and what you're looking for
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
renderPage { step: Register, registrationMode, registrationEmail, registrationDiscord, otherError, submitting, playerOrTeam } =
    [ boardingStep $
        [ boardingHeading "Registration"
        , boardingDescription  """Enter your nickname and password to complete the registration process."""
        ]
        <> case registrationMode of
            Email ->
                [ registrationInput registrationEmail UpdateRegistrationEmail
                , HH.div [HP.style "display: flex; align-items: center; margin: 28px 0;"]
                    [ HH.hr [HP.style "flex: 1 1 auto;"]
                    , HH.span [HP.style "padding: 0 10px"] [HH.text "Or"]
                    , HH.hr [HP.style "flex: 1 1 auto;"]
                    ]
                , HH.button
                    [ HS.class_ "regular-button"
                    , HP.style "width: 100%;"
                    , HP.type_ HP.ButtonButton
                    , HE.onClick $ const CreateWithDiscord
                    ]
                    [ HH.img
                        [ HS.class_ "button-icon"
                        , HP.style "height: 20px; vertical-align: top;"
                        , HP.src "https://coaching.healthygamer.gg/discord-logo-color.svg"
                        ]
                    , HH.text "Create account with Discord"
                    ]
                ]
            Discord ->
                [ registrationInputDiscord registrationDiscord UpdateRegistrationDiscord
                , HH.div [HP.style "display: flex; align-items: center; margin: 28px 0;"]
                    [ HH.hr [HP.style "flex: 1 1 auto;"]
                    , HH.span [HP.style "padding: 0 10px"] [HH.text "Or"]
                    , HH.hr [HP.style "flex: 1 1 auto;"]
                    ]
                , HH.button
                    [ HS.class_ "regular-button"
                    , HP.style "width: 100%;"
                    , HP.type_ HP.ButtonButton
                    , HE.onClick $ const CreateWithEmail
                    ]
                    [ HH.i [ HS.class_ "fas fa-envelope button-icon", HP.style "font-size: 20px;" ] []
                    , HH.text "Create account with Email"
                    ]
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
render state = HH.div_ $ [ boarding $ renderPage state ] <> stickyLeaderboards

sendRequest :: ∀ left. State -> Async left (Either State Preboard.OkContent)
sendRequest state = Async.attempt do
    registration <-
        case state.registrationMode, state.accessToken of
        Email, _ -> Async.right $ inj (Proxy :: _ "email") $ pick state.registrationEmail
        Discord, Just accessToken -> Async.right $ inj (Proxy :: _ "discord")
            {nickname: state.registrationDiscord.nickname, accessToken}
        Discord, Nothing -> do
            window >>= localStorage >>= setItem "preboard" (writeJSON state) # liftEffect
            hardNavigate $ "https://discord.com/api/oauth2/authorize"
                <> "?client_id=1068667687661740052"
                <> "&redirect_uri=https%3A%2F%2Flocalhost%2Fpreboarding%2Fregister"
                <> "&response_type=token"
                <> "&scope=identify"
                <> "&prompt=none"
            Async.left state
    (body :: Preboard.RequestContent) <-
        case state of
        { player
        , playerProfile: profile
        } | Just game <- getGame state.game
          , Just (PlayerOrTeamInput.Player) <- getPlayerOrTeam state.playerOrTeam -> Async.right
            { ilk: 1
            , player: Just $ pick player
            , team: Nothing
            , gameHandle: game.handle
            , playerProfile: Just $ pick profile.details
            , teamProfile: Nothing
            , playerContacts: Just $ pick profile.contacts
            , teamContacts: Nothing
            , registration
            }
        { team
        , teamProfile: profile
        } | Just game <- getGame state.game
          , Just (PlayerOrTeamInput.Team) <- getPlayerOrTeam state.playerOrTeam -> Async.right
            { ilk: 2
            , player: Nothing
            , team: Just $ pick team
            , gameHandle: game.handle
            , playerProfile: Nothing
            , teamProfile: Just $ pick $ Record.insert
                (Proxy :: _ "platforms") profile.details.selectedPlatforms profile.details
            , playerContacts: Nothing
            , teamContacts: Just $ pick profile.contacts
            , registration
            }
        _ -> Async.left state {otherError = true}
    response <- fetchBody (Proxy :: _ Preboard) body # lmap (const $ state {otherError = true})
    onMatch
        { ok: \content -> Async.right content
        , badRequest: \(errors :: Preboard.BadContent) -> Async.left $
            foldl
            (\state error ->
                match
                { team: state # foldl \state' error' -> error' # match
                    { name: const state' { step = Team, team { nameError = true } }
                    , website: const state' { step = Team, team { websiteError = true } }
                    }
                , playerProfile: state # foldl \state' error' -> error' # match
                    { url: \{ key } -> state'
                        { step = if state'.step > PlayerProfile then PlayerProfile else state'.step
                        , playerProfile { details { urlErrors = Array.cons key state'.playerProfile.details.urlErrors } }
                        }
                    , about: const state'
                        { step = if state'.step > PlayerProfile then PlayerProfile else state'.step
                        , playerProfile { details { aboutError = true } }
                        }
                    , ambitions: const state'
                        { step = if state'.step > PlayerProfile then PlayerProfile else state'.step
                        , playerProfile { details { ambitionsError = true } }
                        }
                    }
                , teamProfile: state # foldl \state' error' -> error' # match
                    { platforms: const state'
                        { step = if state'.step > TeamProfile then TeamProfile else state'.step
                        , teamProfile { details { platformsError = true } }
                        }
                    , about: const state'
                        { step = if state'.step > TeamProfile then TeamProfile else state'.step
                        , teamProfile { details { aboutError = true } }
                        }
                    , ambitions: const state'
                        { step = if state'.step > TeamProfile then TeamProfile else state'.step
                        , teamProfile { details { ambitionsError = true } }
                        }
                    }
                , playerContacts: state # foldl \state' error' -> error' # match
                    { discordTag: const state' { step = PlayerProfile, playerProfile { contacts { discordTagError = true } } }
                    , steamId: const state' { step = PlayerProfile, playerProfile { contacts { steamIdError = true } } }
                    , riotId: const state' { step = PlayerProfile, playerProfile { contacts { riotIdError = true } } }
                    , battleTag: const state' { step = PlayerProfile, playerProfile { contacts { battleTagError = true } } }
                    , eaId: const state' { step = PlayerProfile, playerProfile { contacts { eaIdError = true } } }
                    , ubisoftUsername: const state' { step = PlayerProfile, playerProfile { contacts { ubisoftUsernameError = true } } }
                    , psnId: const state' { step = PlayerProfile, playerProfile { contacts { psnIdError = true } } }
                    , gamerTag: const state' { step = PlayerProfile, playerProfile { contacts { gamerTagError = true } } }
                    , friendCode: const state' { step = PlayerProfile, playerProfile { contacts { friendCodeError = true } } }
                    }
                , teamContacts: state # foldl \state' error' -> error' # match
                    { discordTag: const state' { step = TeamProfile, teamProfile { contacts { discordTagError = true } } }
                    , discordServer: const state' { step = TeamProfile, teamProfile { contacts { discordServerError = true } } }
                    , steamId: const state' { step = TeamProfile, teamProfile { contacts { steamIdError = true } } }
                    , riotId: const state' { step = TeamProfile, teamProfile { contacts { riotIdError = true } } }
                    , battleTag: const state' { step = TeamProfile, teamProfile { contacts { battleTagError = true } } }
                    , eaId: const state' { step = TeamProfile, teamProfile { contacts { eaIdError = true } } }
                    , ubisoftUsername: const state' { step = TeamProfile, teamProfile { contacts { ubisoftUsernameError = true } } }
                    , psnId: const state' { step = TeamProfile, teamProfile { contacts { psnIdError = true } } }
                    , gamerTag: const state' { step = TeamProfile, teamProfile { contacts { gamerTagError = true } } }
                    , friendCode: const state' { step = TeamProfile, teamProfile { contacts { friendCodeError = true } } }
                    }
                , registration: state # foldl \state' error' -> error' # match
                    { email: const state' { registrationEmail { emailError = true } }
                    , nickname: const
                        case state.registrationMode of
                        Email -> state' { registrationEmail { nicknameError = true } }
                        Discord -> state' { registrationDiscord { nicknameError = true } }
                    , password: const state' { registrationEmail { passwordError = true } }
                    }
                , emailTaken: const state { registrationEmail { emailTaken = true } }
                , nicknameTaken: const
                    case state.registrationMode of
                    Email -> state { registrationEmail { nicknameTaken = true } }
                    Discord -> state { registrationDiscord { nicknameTaken = true } }
                , other: const state { otherError = true }
                }
                error
            )
            state
            errors
        }
        (const $ Async.left state {otherError = true})
        response

tryToRegister :: forall action slots output left.
    State -> H.HalogenM State action slots output (Async left) Unit
tryToRegister state = do
    H.put state
    eitherStateContent <- H.lift $ sendRequest state
    let trackParams =
            { ilk:
                case getPlayerOrTeam state.playerOrTeam of
                Just PlayerOrTeamInput.Player -> "player"
                Just PlayerOrTeamInput.Team -> "team"
                Nothing -> "unknown"
            , game:
                case getGame state.game of
                Just game -> game.handle
                Nothing -> "unknown"
            }
    let trackSuccess = do
                aliasNickname
                identifyNickname
                track "Preboard" trackParams
    let trackError = track "Preboard error" trackParams

    case eitherStateContent of
        Left nextState -> do
            trackError
            H.put nextState {submitting = false}
        Right {teamHandle: Nothing} -> do
            trackSuccess
            navigate_ "/"
        Right {teamHandle: Just teamHandle} -> do
            trackSuccess
            navigate_ $ "/teams/" <> teamHandle

-- Update state for current history entry so back button doesn't lose previous state.
updateHistoryState :: ∀ monad. MonadEffect monad => State -> monad Unit
updateHistoryState (state :: State) = do
    case state.step of
        Greeting -> replaceState state "/preboarding/start"
        PlayerOrTeam -> replaceState state "/preboarding/player-or-team"
        Player -> replaceState state "/preboarding/player"
        Team -> replaceState state "/preboarding/team"
        Game -> replaceState state "/preboarding/game"
        PlayerProfile -> replaceState state "/preboarding/player-profile"
        TeamProfile -> replaceState state "/preboarding/team-profile"
        Register -> replaceState state "/preboarding/register"

handleAction :: ∀ action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction Initialize = do
    setMeta "Preboarding | TeamTavern" "TeamTavern preboarding."
    -- Check if we came back from Discord sign in page.
    state <- H.get
    case state.accessToken of
        Nothing -> pure unit
        Just _ -> tryToRegister state
handleAction (Receive input) = do
    state <- H.get
    H.put (input # Record.insert (Proxy :: _ "submitting") false)
    guard (input.step /= state.step) $ setMeta "Preboarding | TeamTavern" "TeamTavern preboarding."
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
    state <- H.modify \state -> state { player = Record.merge details state.player }
    updateHistoryState state
handleAction (UpdateTeam details) = do
    state <- H.modify \state -> state { team = Record.merge details state.team }
    updateHistoryState state
handleAction (UpdateGame game) = do
    state <- H.modify _
        { game = Selected $ Just game
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
handleAction (UpdateRegistrationEmail registration) = do
    state <- H.modify \state -> state
        { registrationEmail = Record.merge registration state.registrationEmail }
    updateHistoryState state
handleAction (UpdateRegistrationDiscord registration) = do
    state <- H.modify \state -> state
        { registrationDiscord = Record.merge registration state.registrationDiscord }
    updateHistoryState state
handleAction SetUpAccount = do
    state <- H.gets (_
        { submitting = true
        , otherError = false
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
        , registrationEmail
            { emailError = false
            , nicknameError = false
            , passwordError = false
            , emailTaken = false
            , nicknameTaken = false
            }
        , registrationDiscord
            { nicknameError = false
            , nicknameTaken = false
            }
        }
    )
    tryToRegister state
handleAction CreateWithEmail =
    H.modify_ _ {registrationMode = Email}
handleAction CreateWithDiscord =
    H.modify_ _ {registrationMode = Discord}

component :: ∀ query output left.
    H.Component query Input output (Async left)
component = H.mkComponent
    { initialState: Record.insert (Proxy :: _ "submitting") false
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

preboarding :: ∀ action slots left.
    Input -> HH.ComponentHTML action (preboarding :: Slot___ | slots) (Async left)
preboarding input = HH.slot (Proxy :: _ "preboarding") unit component input absurd

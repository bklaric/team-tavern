module TeamTavern.Client.Pages.Onboarding (Step(..), Input, Slot, onboarding) where

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
import Data.Maybe (Maybe(..), isJust)
import Data.Options ((:=))
import Data.Symbol (SProxy(..))
import Data.Variant (match)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.Button (primaryButton_, secondaryButton_)
import TeamTavern.Client.Components.Player.PlayerFormInput (playerFormInput)
import TeamTavern.Client.Components.Player.PlayerFormInput as EnterPlayerDetails
import TeamTavern.Client.Components.Player.ProfileFormInput (profileFormInput)
import TeamTavern.Client.Components.Player.ProfileFormInput as EnterPlayerProfileDetails
import TeamTavern.Client.Pages.Onboarding.SelectGame (selectGame)
import TeamTavern.Client.Pages.Onboarding.SelectGame as SelectGame
import TeamTavern.Client.Script.Cookie (getPlayerNickname)
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Navigate (navigate, navigate_)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Server.Wizard.Onboard as Onboard

data Step
    = Greeting
    | PlayerOrTeam
    | PlayerDetails
    | TeamDetails
    | Game
    | PlayerProfileDetails
    | TeamProfileDetails

type Input = { step :: Step }

type State =
    { step :: Step
    , confirmSkip :: Boolean
    , nickname :: String
    , playerDetails :: EnterPlayerDetails.Input
    , game :: SelectGame.Input
    , playerProfileDetails :: EnterPlayerProfileDetails.Input
    , otherError :: Boolean
    , submitting :: Boolean
    }

data Action
    = Initialize
    | Receive Input
    | Skip
    | ConfirmSkip
    | SetStep Step
    | UpdatePlayerDetails EnterPlayerDetails.Output
    | UpdateGame SelectGame.Output
    | UpdatePlayerProfileDetails EnterPlayerProfileDetails.Output
    | SetUpAccount

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots slots =
    ( playerFormInput :: EnterPlayerDetails.Slot
    , profileFormInput :: EnterPlayerProfileDetails.Slot
    , selectGame :: SelectGame.Slot
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
        , primaryButton_ "Let's go" $ SetStep PlayerDetails
        ]
    ]
renderPage { step: PlayerOrTeam } =
    [

    ]
renderPage { step: PlayerDetails, playerDetails } =
    [ onboardingStep
        [ onboardingHeading "Player details"
        , onboardingDescription  """Enter details about yourself so your new bruh gamer friends
                can find you, bruh. Fill out as much as you can to ensure the
                bruhest gamers find you. All fields are optional, bruh."""
        , playerFormInput playerDetails (Just <<< UpdatePlayerDetails)
        ]
    , onboardingButtons
        [ secondaryButton_ "Back" $ SetStep Greeting
        , primaryButton_ "Next" $ SetStep Game
        ]
    ]
renderPage { step: TeamDetails } = []
renderPage { step: Game, game } =
    [ onboardingStep
        [ onboardingHeading "Game"
        , onboardingDescription  """Select a game to create your first profile muhfugga."""
        , selectGame game (Just <<< UpdateGame)
        ]
    , onboardingButtons
        [ secondaryButton_ "Back" $ SetStep PlayerDetails
        , primaryButton_ "Next" $ SetStep PlayerProfileDetails
        ]
    ]
renderPage { step: PlayerProfileDetails, playerProfileDetails, otherError, submitting } =
    [ onboardingStep
        [ onboardingHeading "Profile details"
        , onboardingDescription  """Enter details about your gameplay. Fill out everything."""
        , profileFormInput playerProfileDetails UpdatePlayerProfileDetails
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
renderPage { step: TeamProfileDetails } = []

render :: forall slots left.
    State -> HH.ComponentHTML Action (ChildSlots slots) (Async left)
render state = onboarding' $ renderPage state

sendRequest :: forall left. State -> Async left (Maybe (Either Onboard.BadRequestContent Unit))
sendRequest (state :: State) = Async.unify do
    (body :: Onboard.RequestBody) <-
        case state.game, state.playerDetails, state.playerProfileDetails of
        Just game, personalDetails, profileDetails -> Async.right
            { handle: game.handle
            , personalDetails:
                { birthday: personalDetails.birthday
                , location: personalDetails.location
                , languages: personalDetails.languages
                , microphone: personalDetails.microphone
                , discordTag: personalDetails.discordTag
                , timezone: personalDetails.timezone
                , weekdayFrom: personalDetails.weekdayFrom
                , weekdayTo: personalDetails.weekdayTo
                , weekendFrom: personalDetails.weekendFrom
                , weekendTo: personalDetails.weekendTo
                , about: personalDetails.about
                }
            , profileDetails:
                { fieldValues: profileDetails.fieldValues # Array.filter \{ url, optionKey, optionKeys } ->
                    isJust url || isJust optionKey || isJust optionKeys
                , newOrReturning: profileDetails.newOrReturning
                , ambitions: profileDetails.ambitions
                }
            }
        _, _, _ -> Async.left Nothing
    response <-
        Fetch.fetch "/api/wizard/onboard"
        ( Fetch.method := POST
        <> Fetch.body := Json.writeJSON body
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const Nothing)
    content :: Either Onboard.BadRequestContent Unit <-
        case FetchRes.status response of
        204 -> pure $ Right unit
        400 -> FetchRes.text response >>= JsonAsync.readJSON # bimap (const Nothing) Left
        _ -> Async.left Nothing
    pure $ Just content

handleAction :: forall action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    case state.step of
        Greeting -> pure unit
        _ -> navigate_ "/"
    nickname <- getPlayerNickname
    case nickname of
        Just nickname' -> H.modify_ _ { nickname = nickname' }
        Nothing -> navigate_ "/"
    H.liftEffect do
        setMetaTitle "Onboarding | TeamTavern"
        setMetaDescription "TeamTavern onboarding."
        setMetaUrl
handleAction (Receive { step }) =
    H.modify_ _ { step = step, confirmSkip = false }
handleAction Skip =
    H.modify_ _ { confirmSkip = true }
handleAction ConfirmSkip =
    navigate_ "/"
handleAction (SetStep step) =
    H.liftEffect
        case step of
        Greeting -> navigate { firstSignIn: true } "/onboarding/start"
        PlayerOrTeam -> navigate_ "/onboarding/player-or-team"
        PlayerDetails -> navigate_ "/onboarding/player"
        TeamDetails -> navigate_ "/onboarding/team"
        Game -> navigate_ "/onboarding/game"
        PlayerProfileDetails -> navigate_ "/onboarding/player-profile"
        TeamProfileDetails -> navigate_ "/onboarding/team-profile"
handleAction (UpdatePlayerDetails details) =
    H.modify_ \state -> state
        { playerDetails = state.playerDetails
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
handleAction (UpdateGame game) =
    H.modify_ \state -> state
        { game = Just game
        , playerProfileDetails = state.playerProfileDetails
            { fields = game.fields
            , fieldValues = []
            , newOrReturning = false
            , ambitions = ""
            }
        }
handleAction (UpdatePlayerProfileDetails details) =
    H.modify_ \state -> state
        { playerProfileDetails = state.playerProfileDetails
            { fieldValues = details.fieldValues
            , newOrReturning = details.newOrReturning
            , ambitions = details.ambitions
            }
        }
handleAction SetUpAccount = do
    currentState <- H.modify _ { submitting = true }
    response <- H.lift $ sendRequest currentState
    case response of
        Just (Right _) -> navigate_ "/"
        Just (Left errors) -> H.put $
            foldl
            (\state error ->
                match
                { invalidDiscordTag: const $ state
                    { step = PlayerDetails
                    , playerDetails = state.playerDetails
                        { discordTagError = true }
                    }
                , invalidAbout: const $ state
                    { step = PlayerDetails
                    , playerDetails = state.playerDetails
                        { aboutError = true }}
                , url: \{ key } -> state
                    { playerProfileDetails = state.playerProfileDetails
                        { urlErrors = Array.cons key state.playerProfileDetails.urlErrors }
                    }
                , missing: \{ key } -> state
                    { playerProfileDetails = state.playerProfileDetails
                        { missingErrors = Array.cons key state.playerProfileDetails.missingErrors }
                    }
                , ambitions: const $ state
                    { playerProfileDetails = state.playerProfileDetails
                        { ambitionsError = true }
                    }
                }
                error
            )
            (currentState
                { submitting = false
                , playerDetails = currentState.playerDetails
                    { discordTagError = false, aboutError = false }
                , playerProfileDetails = currentState.playerProfileDetails
                    { urlErrors = [], missingErrors = [], ambitionsError = false }
                , otherError = false
                })
            errors
        Nothing -> H.put currentState
            { submitting = false
            , playerDetails = currentState.playerDetails { discordTagError = false }
            , playerProfileDetails = currentState.playerProfileDetails
                { urlErrors = [], missingErrors = [], ambitionsError = false }
            , otherError = true
            }

component :: forall query output left.
    H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: \{ step } ->
        { step
        , nickname: ""
        , confirmSkip: false
        , playerDetails: EnterPlayerDetails.emptyInput
        , game: Nothing
        , playerProfileDetails: EnterPlayerProfileDetails.emptyInput []
        , otherError: false
        , submitting: false
        }
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

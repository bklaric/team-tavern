module TeamTavern.Client.Pages.Home.Wizard where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (filter, foldl)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Options ((:=))
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Variant (match)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.Welcome as Welcome
import TeamTavern.Client.Pages.Home.Wizard.EnterGeneralPlayerDetails (enterGeneralPlayerDetails)
import TeamTavern.Client.Pages.Home.Wizard.EnterGeneralPlayerDetails as EnterGeneralPlayerDetails
import TeamTavern.Client.Pages.Home.Wizard.EnterProfilePlayerDetails (enterProfilePlayerDetails)
import TeamTavern.Client.Pages.Home.Wizard.EnterProfilePlayerDetails as EnterProfilePlayerDetails
import TeamTavern.Client.Pages.Home.Wizard.EnterRegistrationDetails (enterRegistrationDetails)
import TeamTavern.Client.Pages.Home.Wizard.EnterRegistrationDetails as EnterRegistrationDetails
import TeamTavern.Client.Pages.Home.Wizard.SelectGame (selectGame)
import TeamTavern.Client.Pages.Home.Wizard.SelectGame as SelectGame
import TeamTavern.Client.Pages.Home.Wizard.Shared (Ilk(..))
import TeamTavern.Client.Script.Navigate (navigate)
import TeamTavern.Client.Snippets.ErrorClasses (otherErrorClass)
import TeamTavern.Server.Game.View.SendResponse (OkContent)
import TeamTavern.Server.Wizard.CreateAccount as CreateAccount

type Handle = String

type Game = OkContent

data Step
    = SelectGame
    | EnterGeneralPlayerDetails
    | EnterProfilePlayerDetails
    | EnterRegistrationDetails

type Input = { ilk :: Ilk }

type Output = {}

type State =
    { ilk :: Ilk
    , step :: Step
    , handle :: Maybe String
    , game :: Maybe Game
    , generalPlayerDetailsInput :: EnterGeneralPlayerDetails.Input
    , profilePlayerDetailsInput :: EnterProfilePlayerDetails.Input
    , registrationDetailsInput :: EnterRegistrationDetails.Input
    , otherError :: Boolean
    , submitting :: Boolean
    }

data Action
    = TakeSelectedGame Handle
    | TakeGeneralPlayerDetails EnterGeneralPlayerDetails.Output
    | TakeProfilePlayerDetails EnterProfilePlayerDetails.Output
    | TakeRegistrationDetails EnterRegistrationDetails.Output
    | SetStep Step
    | Submit

type Slot = H.Slot (Const Void) (Modal.Output Output) Unit

type Slots slots =
    ( selectGame :: SelectGame.Slot
    , enterGeneralPlayerDetails :: EnterGeneralPlayerDetails.Slot
    , enterProfilePlayerDetails :: EnterProfilePlayerDetails.Slot
    , enterRegistrationDetails :: EnterRegistrationDetails.Slot
    | slots )

profileIlk :: Ilk -> String
profileIlk Player = "player"
profileIlk Team = "team"

render :: forall slots left.
    State -> HH.ComponentHTML Action (Slots slots) (Async left)
render state @ { step, ilk } =
    HH.div
    [ HP.class_ $ HH.ClassName "wide-single-form-container wizard" ]
    case step of
        SelectGame ->
            [ HH.h2 [ HP.class_ $ HH.ClassName "form-heading" ]
                [ HH.text "Select game" ]
            , HH.p [ HP.class_ $ HH.ClassName "form-subheading" ]
                [ HH.text $ "Select one of the games below to start creating your "
                    <> profileIlk ilk <> " profile"
                ]
            , selectGame { ilk, selectedHandle: state.handle } (Just <<< TakeSelectedGame)
            , HH.div [ HP.class_ $ HH.ClassName "form-navigation" ]
                [ HH.button
                    [ HP.class_ $ HH.ClassName "form-next-button"
                    , HP.disabled $ isNothing state.handle
                    , HE.onClick $ const $ Just $ SetStep EnterGeneralPlayerDetails
                    ]
                    [ HH.text "Next" ]
                ]
            ]
        EnterGeneralPlayerDetails ->
            [ HH.h2 [ HP.class_ $ HH.ClassName "form-heading" ]
                [ HH.text "Enter player details" ]
            , HH.p [ HP.class_ $ HH.ClassName "form-subheading" ]
                [ HH.text "Fill out general details about yourself." ]
            , enterGeneralPlayerDetails state.generalPlayerDetailsInput (Just <<< TakeGeneralPlayerDetails)
            , HH.div [ HP.class_ $ HH.ClassName "form-navigation" ]
                [ HH.button
                    [ HP.class_ $ HH.ClassName "form-next-button"
                    , HE.onClick $ const $ Just $ SetStep EnterProfilePlayerDetails
                    ]
                    [ HH.text "Next" ]
                , HH.button
                    [ HP.class_ $ HH.ClassName "form-back-button"
                    , HE.onClick $ const $ Just $ SetStep SelectGame
                    ]
                    [ HH.text "Back" ]
                ]
            ]
        EnterProfilePlayerDetails ->
            [ HH.h2 [ HP.class_ $ HH.ClassName "form-heading" ]
                [ HH.text "Enter profile details" ]
            , HH.p [ HP.class_ $ HH.ClassName "form-subheading" ]
                [ HH.text "Fill out game related details about yourself." ]
            , enterProfilePlayerDetails state.profilePlayerDetailsInput (Just <<< TakeProfilePlayerDetails)
            , HH.div [ HP.class_ $ HH.ClassName "form-navigation" ]
                [ HH.button
                    [ HP.class_ $ HH.ClassName "form-next-button"
                    , HE.onClick $ const $ Just $ SetStep EnterRegistrationDetails
                    ]
                    [ HH.text "Next" ]
                , HH.button
                    [ HP.class_ $ HH.ClassName "form-back-button"
                    , HE.onClick $ const $ Just $ SetStep EnterGeneralPlayerDetails
                    ]
                    [ HH.text "Back" ]
                ]
            ]
        EnterRegistrationDetails ->
            [ HH.h2 [ HP.class_ $ HH.ClassName "form-heading" ]
                [ HH.text "Enter registration details" ]
            , enterRegistrationDetails state.registrationDetailsInput (Just <<< TakeRegistrationDetails)
            , HH.p
                [ HP.class_ $ otherErrorClass state.otherError ]
                [ HH.text "Something unexpected went wrong! Please try again later." ]
            , HH.div [ HP.class_ $ HH.ClassName "form-navigation" ]
                [ HH.button
                    [ HP.class_ $ HH.ClassName "form-next-button"
                    , HP.disabled state.submitting
                    , HE.onClick $ const $ Just $ Submit
                    ]
                    [ HH.text if state.submitting then "Submitting..." else "Submit" ]
                , HH.button
                    [ HP.class_ $ HH.ClassName "form-back-button"
                    , HP.disabled state.submitting
                    , HE.onClick $ const $ Just $ SetStep EnterProfilePlayerDetails
                    ]
                    [ HH.text "Back" ]
                ]
            ]

loadGame :: forall left. String -> Async left (Maybe Game)
loadGame handle = Async.unify do
    response <-
        Fetch.fetch_ ("/api/games/by-handle/" <> handle)
        # lmap (const Nothing)
    content <-
        case FetchRes.status response of
        200 -> FetchRes.text response >>= JsonAsync.readJSON # lmap (const Nothing)
        _ -> Async.left Nothing
    pure $ Just content

sendRequest :: forall left. State -> Async left (Maybe (Either CreateAccount.BadRequestContent CreateAccount.OkContent))
sendRequest (state :: State) = Async.unify do
    (body :: CreateAccount.RequestBody) <-
        case state.handle, state.generalPlayerDetailsInput, state.profilePlayerDetailsInput, state.registrationDetailsInput of
        Just handle, personalDetails, profileDetails, registrationDetails -> Async.right
            { handle
            , personalDetails:
                { birthday: if String.null personalDetails.birthday then Nothing else Just personalDetails.birthday
                , location: personalDetails.location
                , languages: personalDetails.languages
                , microphone: personalDetails.microphone
                , discordTag: if String.null personalDetails.discordTag then Nothing else Just personalDetails.discordTag
                , timezone: personalDetails.timezone
                , weekdayFrom: if String.null personalDetails.weekdayFrom then Nothing else Just personalDetails.weekdayFrom
                , weekdayTo: if String.null personalDetails.weekdayTo then Nothing else Just personalDetails.weekdayTo
                , weekendFrom: if String.null personalDetails.weekendFrom then Nothing else Just personalDetails.weekendFrom
                , weekendTo: if String.null personalDetails.weekendTo then Nothing else Just personalDetails.weekendTo
                }
            , profileDetails:
                { fieldValues: profileDetails.fieldValues # filter \{ url, optionKey, optionKeys } ->
                    isJust url || isJust optionKey || isJust optionKeys
                , newOrReturning: profileDetails.newOrReturning
                , summary: profileDetails.summary
                }
            , registrationDetails:
                { email: registrationDetails.email
                , nickname: registrationDetails.nickname
                , password: registrationDetails.password
                }
            }
        _, _, _, _ -> Async.left Nothing
    response <-
        Fetch.fetch "/api/wizard"
        ( Fetch.method := POST
        <> Fetch.body := Json.writeJSON body
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const Nothing)
    content :: Either CreateAccount.BadRequestContent CreateAccount.OkContent <-
        case FetchRes.status response of
        200 -> FetchRes.text response >>= JsonAsync.readJSON # bimap (const Nothing) Right
        400 -> FetchRes.text response >>= JsonAsync.readJSON # bimap (const Nothing) Left
        _ -> Async.left Nothing
    pure $ Just content

profilePlayerInput :: Game -> EnterProfilePlayerDetails.Input
profilePlayerInput game =
    { fields: game.fields
    , fieldValues: []
    , newOrReturning: false
    , summary: ""
    , urlErrors: []
    , missingErrors: []
    , summaryError: false
    }

profilePlayerEmptyInput :: EnterProfilePlayerDetails.Input
profilePlayerEmptyInput =
    { fields: []
    , fieldValues: []
    , newOrReturning: false
    , summary: ""
    , urlErrors: []
    , missingErrors: []
    , summaryError: false
    }

generalPlayerEmptyInput :: EnterGeneralPlayerDetails.Input
generalPlayerEmptyInput =
    { birthday: ""
    , location: Nothing
    , languages: []
    , microphone: false
    , discordTag: ""
    , timezone: Nothing
    , weekdayFrom: ""
    , weekdayTo: ""
    , weekendFrom: ""
    , weekendTo: ""
    , discordTagError: false
    }


handleAction :: forall action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction (TakeSelectedGame handle) = do
    H.modify_ _ { handle = Just handle }
handleAction (TakeGeneralPlayerDetails details) =
    H.modify_ \state -> state
        { generalPlayerDetailsInput = state.generalPlayerDetailsInput
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
            }
        }
handleAction (TakeProfilePlayerDetails details) =
    H.modify_ \state -> state
        { profilePlayerDetailsInput = state.profilePlayerDetailsInput
            { fieldValues = details.fieldValues
            , newOrReturning = details.newOrReturning
            , summary = details.summary
            }
        }
handleAction (TakeRegistrationDetails details) =
    H.modify_ \state -> state
        { registrationDetailsInput = state.registrationDetailsInput
            { email = details.email
            , nickname = details.nickname
            , password = details.password
            }
        }
handleAction (SetStep step) = do
    state <- H.get
    case state.step, state.handle of
        SelectGame, Just handle -> do
            game <- H.lift $ loadGame handle
            case game of
                Just game' -> H.modify_ _
                    { step = step
                    , game = Just game'
                    , profilePlayerDetailsInput = profilePlayerInput game'
                    }
                Nothing -> pure unit
        _, _ -> H.modify_ _ { step = step }
handleAction Submit = do
    currentState <- H.modify \state -> state
        { generalPlayerDetailsInput = state.generalPlayerDetailsInput
            { discordTagError = false }
        , profilePlayerDetailsInput = state.profilePlayerDetailsInput
            { urlErrors = []
            , missingErrors = []
            , summaryError = false
            }
        , registrationDetailsInput = state.registrationDetailsInput
            { emailError = false
            , nicknameError = false
            , passwordError = false
            , emailTaken = false
            , nicknameTaken = false
            }
        , submitting = true
        }
    response <- H.lift $ sendRequest currentState
    case response, currentState.game of
        Just (Right okContent), Just { title } ->
            H.liftEffect $ navigate
                ((Record.insert (SProxy :: SProxy "profile") (Just { title }) okContent) :: Welcome.Input)
                "/welcome"
        _, _ -> pure unit
    let nextState = case response of
            Nothing -> currentState { otherError = true }
            Just (Left badContent) -> badContent # match
                { invalidBody:
                    foldl
                    (\state bodyError ->
                        match
                        { invalidDiscordTag: const $ state
                            { generalPlayerDetailsInput = state.generalPlayerDetailsInput
                                { discordTagError = true }
                            }
                        , invalidProfile:
                            foldl
                            (\state' profileError ->
                                match
                                { invalidUrl: \{ fieldKey } -> state'
                                    { profilePlayerDetailsInput = state'.profilePlayerDetailsInput
                                        { urlErrors = Array.cons fieldKey state'.profilePlayerDetailsInput.urlErrors }
                                    }
                                , missing: \{ fieldKey } -> state'
                                    { profilePlayerDetailsInput = state'.profilePlayerDetailsInput
                                        { missingErrors = Array.cons fieldKey state'.profilePlayerDetailsInput.missingErrors }
                                    }
                                , summary: const $ state'
                                    { profilePlayerDetailsInput = state'.profilePlayerDetailsInput
                                        { summaryError = true }
                                    }
                                }
                                profileError
                            )
                            state
                        , invalidRegistration:
                            foldl
                            (\state' registrationError ->
                                match
                                { invalidEmail: const $ state'
                                    { registrationDetailsInput = state'.registrationDetailsInput
                                        { emailError = true }
                                    }
                                , invalidNickname: const $ state'
                                    { registrationDetailsInput = state'.registrationDetailsInput
                                        { nicknameError = true }
                                    }
                                , invalidPassword: const $ state'
                                    { registrationDetailsInput = state'.registrationDetailsInput
                                        { passwordError = true }
                                    }
                                }
                                registrationError
                            )
                            state
                        }
                        bodyError
                    )
                    currentState
                , emailTaken: const $ currentState
                    { registrationDetailsInput = currentState.registrationDetailsInput
                        { emailTaken = true }
                    }
                , nicknameTaken: const $ currentState
                    { registrationDetailsInput = currentState.registrationDetailsInput
                        { nicknameTaken = true }
                    }
                }
            Just (Right okContent) -> currentState
    H.put nextState
        { submitting = false
        , step =
            if nextState.generalPlayerDetailsInput.discordTagError
            then EnterGeneralPlayerDetails
            else if (not $ Array.null nextState.profilePlayerDetailsInput.urlErrors)
                || (not $ Array.null nextState.profilePlayerDetailsInput.missingErrors)
                || nextState.profilePlayerDetailsInput.summaryError
            then EnterProfilePlayerDetails
            else EnterRegistrationDetails
        }

component :: forall query output left.
    H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: \{ ilk } ->
        { ilk
        , step: SelectGame
        , handle: Nothing
        , game: Nothing
        , generalPlayerDetailsInput: generalPlayerEmptyInput
        , profilePlayerDetailsInput: profilePlayerEmptyInput
        , registrationDetailsInput: EnterRegistrationDetails.emptyInput
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction }
    }

wizard
    :: forall action slots left
    .  Input
    -> (Modal.Output Output -> Maybe action)
    -> HH.ComponentHTML action (wizard :: Slot | slots) (Async left)
wizard input handleOutput = HH.slot
    (SProxy :: SProxy "wizard") unit
    (Modal.component component) input handleOutput

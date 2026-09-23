module TeamTavern.Client.Pages.SignIn (signIn) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array.NonEmpty as Nea
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (null, trim)
import Data.Tuple.Nested ((/\))
import Data.Variant (inj, match, onMatch)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Button (Size(..), Weight(..), button)
import TeamTavern.Client.Components.Divider (rule)
import TeamTavern.Client.Components.Flow (flow, flowError, flowLead, flowLink, formTight, submitButton, textField)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Pages.Post.Register (Publishing, publishing, publishingPost)
import TeamTavern.Client.Script.Back (authPath, readBack)
import TeamTavern.Client.Script.Discord (authorizeWithDiscord, takeDiscordReturn)
import TeamTavern.Client.Script.Navigate (navigateReplace_, navigate_)
import TeamTavern.Client.Shared.AccountErrors (nicknameInvalid, nicknameTaken, somethingWrong)
import TeamTavern.Client.Shared.Fetch (fetchBody)
import TeamTavern.Client.Shared.SignedIn (signedIn)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Player.RegisterPlayer (RegisterPlayer)
import TeamTavern.Routes.Session.StartSession (StartSession)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)

-- Discord sends every player it signs in back here. One with an account is
-- signed in and goes on; one without picks a nickname, which is the rest of
-- registering with Discord.
data Screen
    = Password
    | Discord
    | Nickname { accessToken :: String }

-- | `publishing` is the post the page goes on to publish when it is the
-- | register step of posting, and `post` how that post is named.
type State =
    { screen :: Screen
    , back :: String
    , publishing :: Maybe Publishing
    , post :: Maybe String
    , emailOrNickname :: String
    , password :: String
    , nickname :: String
    , errors ::
        { emailOrNickname :: Maybe String
        , password :: Maybe String
        , nickname :: Maybe String
        , form :: Maybe String
        }
    , sending :: Boolean
    }

noErrors :: { emailOrNickname :: Maybe String, password :: Maybe String, nickname :: Maybe String, form :: Maybe String }
noErrors = { emailOrNickname: Nothing, password: Nothing, nickname: Nothing, form: Nothing }

initialState :: State
initialState =
    { screen: Password
    , back: "/"
    , publishing: Nothing
    , post: Nothing
    , emailOrNickname: ""
    , password: ""
    , nickname: ""
    , errors: noErrors
    , sending: false
    }

component :: ∀ query input output left. H.Component query input output (Async left)
component = Hooks.component \_ _ -> Hooks.do
    state /\ stateId <- Hooks.useState initialState

    let set = Hooks.modify_ stateId
        failWith errors = set _ { sending = false, errors = errors }

        startDiscordSession accessToken back = do
            set _ { screen = Discord, back = back, publishing = publishing back }
            result <- H.lift $ Async.attempt $ fetchBody (Proxy :: _ StartSession)
                (inj (Proxy :: _ "discord") { accessToken })
            case result of
                Right response -> response # onMatch
                    { noContent: const $ navigateReplace_ back
                    , badRequest: onMatch
                        { unknownDiscord: \{ nickname } ->
                            set _ { screen = Nickname { accessToken }, nickname = nickname }
                        }
                        (const $ failWith noErrors { form = Just somethingWrong })
                    , forbidden: const $ navigateReplace_ back
                    }
                    (const $ failWith noErrors { form = Just somethingWrong })
                Left _ -> failWith noErrors { form = Just somethingWrong }

        submitPassword event = do
            H.liftEffect $ preventDefault event
            let errors = noErrors
                    { emailOrNickname =
                        if null $ trim state.emailOrNickname then Just "Enter your email or nickname." else Nothing
                    , password = if null state.password then Just "Enter your password." else Nothing
                    }
            if isJust errors.emailOrNickname || isJust errors.password
            then failWith errors
            else do
                set _ { sending = true, errors = noErrors }
                result <- H.lift $ Async.attempt $ fetchBody (Proxy :: _ StartSession)
                    (inj (Proxy :: _ "password")
                        { emailOrNickname: trim state.emailOrNickname, password: state.password })
                case result of
                    Right response -> response # onMatch
                        { noContent: const $ navigate_ state.back
                        , badRequest: match
                            { unknownPlayer: const $ failWith noErrors
                                { emailOrNickname = Just "No account exists with this email or nickname." }
                            , wrongPassword: const $ failWith noErrors
                                { password = Just "Entered password is incorrect." }
                            , unknownDiscord: const $ failWith noErrors { form = Just somethingWrong }
                            }
                        , forbidden: const $ navigate_ state.back
                        }
                        (const $ failWith noErrors { form = Just somethingWrong })
                    Left _ -> failWith noErrors { form = Just somethingWrong }

        submitNickname accessToken event = do
            H.liftEffect $ preventDefault event
            if null $ trim state.nickname
            then failWith noErrors { nickname = Just "Choose a nickname." }
            else do
                set _ { sending = true, errors = noErrors }
                result <- H.lift $ Async.attempt $ fetchBody (Proxy :: _ RegisterPlayer)
                    (inj (Proxy :: _ "discord") { nickname: trim state.nickname, accessToken })
                case result of
                    Right response -> response # onMatch
                        { noContent: const $ navigateReplace_ state.back
                        , badRequest: match
                            { registration: \errors -> failWith noErrors
                                { nickname = errors # Nea.head # match
                                    { email: const $ Just somethingWrong
                                    , nickname: const $ Just nicknameInvalid
                                    , password: const $ Just somethingWrong
                                    }
                                }
                            , nicknameTaken: const $ failWith noErrors { nickname = Just nicknameTaken }
                            , discordTaken: const $ failWith noErrors
                                { form = Just "This Discord account already has a TeamTavern account. Sign in with Discord instead." }
                            , emailTaken: const $ failWith noErrors { form = Just somethingWrong }
                            }
                        , forbidden: const $ navigateReplace_ state.back
                        }
                        (const $ failWith noErrors { form = Just somethingWrong })
                    Left _ -> failWith noErrors { form = Just somethingWrong }

    Hooks.useLifecycleEffect do
        discordReturn <- takeDiscordReturn
        case discordReturn of
            Just { accessToken, back } -> startDiscordSession accessToken back
            Nothing -> do
                back <- readBack
                set _ { back = back, publishing = publishing back }
                void $ Hooks.fork do
                    signedIn' <- H.lift signedIn
                    when signedIn' $ navigateReplace_ back
                for_ (publishing back) \publishing' -> void $ Hooks.fork do
                    post <- H.lift $ publishingPost publishing'
                    set _ { post = post }
        pure Nothing

    let formError = case state.errors.form of
            Just error -> [ flowError error ]
            Nothing -> []

    Hooks.pure case state.screen of
        Password -> flow $
            [ HH.h1_ [ HH.text if isJust state.publishing then "Sign in to publish" else "Sign in" ] ]
            <> (if isJust state.publishing
                then [ flowLead $ "Your " <> fromMaybe "post" state.post
                    <> " goes live as soon as you're signed in. Nothing you wrote is lost." ]
                else [])
            <>
            [ button Outline Regular (authorizeWithDiscord state.back)
                [ Icons.discord, HH.text "Continue with Discord" ]
            , rule "or"
            , formTight submitPassword $
                [ textField
                    { id: "signin-email", label: "Email or nickname"
                    , type_: HP.InputText, autocomplete: HP.AutocompleteUsername
                    , hint: Nothing, error: state.errors.emailOrNickname
                    , value: state.emailOrNickname, onInput: \value -> set _ { emailOrNickname = value }
                    }
                , textField
                    { id: "signin-password", label: "Password"
                    , type_: HP.InputPassword, autocomplete: HP.AutocompleteCurrentPassword
                    , hint: Nothing, error: state.errors.password
                    , value: state.password, onInput: \value -> set _ { password = value }
                    }
                ]
                <> formError
                <> [ submitButton state.sending if isJust state.publishing then "Sign in and publish" else "Sign in" ]
            , HH.p [ HS.class_ "muted" ] [ flowLink (authPath "/forgot-password" state.back) "Forgot password?" ]
            , HH.p [ HS.class_ "muted" ]
                [ HH.text "New here? ", flowLink (authPath "/signup" state.back) "Create an account" ]
            ]
        Discord -> flow $
            [ HH.h1_ [ HH.text "Sign in" ]
            , flowLead "Signing you in with Discord…"
            ]
            <> formError
        Nickname { accessToken } -> flow
            [ HH.h1_ [ HH.text "Pick a nickname" ]
            , flowLead "It's shown on your posts and your messages. We took it from Discord; change it if you like."
            , formTight (submitNickname accessToken) $
                [ textField
                    { id: "signin-nickname", label: "Nickname"
                    , type_: HP.InputText, autocomplete: HP.AutocompleteNickname
                    , hint: Nothing, error: state.errors.nickname
                    , value: state.nickname, onInput: \value -> set _ { nickname = value }
                    }
                ]
                <> formError
                <> [ submitButton state.sending if isJust state.publishing then "Publish post" else "Continue" ]
            ]

signIn :: ∀ action slots left. H.ComponentHTML action (signIn :: Slot___ | slots) (Async left)
signIn = HH.slot_ (Proxy :: _ "signIn") unit component unit

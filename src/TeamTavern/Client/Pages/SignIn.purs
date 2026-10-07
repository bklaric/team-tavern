module TeamTavern.Client.Pages.SignIn (signIn) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array.NonEmpty (elem)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..), null, stripPrefix, trim)
import Data.Tuple.Nested ((/\))
import Data.Variant (inj, match, onMatch)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Button (Size(..), Weight(..), button)
import TeamTavern.Client.Components.Divider (rule)
import TeamTavern.Client.Components.ContactPanel (contacting, contactingOwner)
import TeamTavern.Client.Components.Flow (flow, flowError, flowLead, flowLink, formTight, submitButton, textField)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Pages.Post.Register (Publishing, publishing, publishingPost)
import TeamTavern.Client.Script.Back (authPath, readBack)
import TeamTavern.Client.Script.Discord (authorizeWithDiscord, keepSwitchToken, takeDiscordReturn)
import TeamTavern.Client.Script.Navigate (navigateReplace_, navigate_, replaceState)
import TeamTavern.Client.Script.Steam (authorizeWithSteam, keepSwitchAssertion, takeSteamReturn)
import TeamTavern.Client.Shared.AccountErrors (emailError, emailInvalid, nicknameInvalid, nicknameTaken, somethingWrong)
import TeamTavern.Client.Shared.Fetch (expecting, fetchBody)
import TeamTavern.Client.Shared.Me (fetchMe)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Player.RegisterPlayer (RegisterPlayer)
import TeamTavern.Routes.Session.StartSession (StartSession)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)

-- Discord and Steam send every player they sign in back here. One with an
-- account is signed in and goes on; one without picks a nickname, which is the
-- rest of registering. Steam gives no address, so a Steam player types one in.
data Screen
    = Password
    | Returning String
    | Nickname Registering

-- `offered` is whether Steam gave a profile name to offer as the nickname.
data Registering
    = WithDiscord { accessToken :: String }
    | WithSteam { ticket :: String, offered :: Boolean }

-- | `publishing` is the post the page goes on to publish when it is the
-- | register step of posting, and `post` how that post is named. `contacting`
-- | is whose post the page returns to contact.
type State =
    { screen :: Screen
    , back :: String
    , publishing :: Maybe Publishing
    , post :: Maybe String
    , contacting :: Maybe String
    , emailOrNickname :: String
    , password :: String
    , nickname :: String
    , email :: String
    , errors :: Errors
    , sending :: Boolean
    }

-- `returning` is what went wrong signing in with Discord or Steam, which the
-- sign-in screen shows above their buttons.
type Errors =
    { emailOrNickname :: Maybe String
    , password :: Maybe String
    , nickname :: Maybe String
    , email :: Maybe String
    , form :: Maybe String
    , returning :: Maybe String
    }

noErrors :: Errors
noErrors =
    { emailOrNickname: Nothing, password: Nothing, nickname: Nothing, email: Nothing, form: Nothing, returning: Nothing }

initialState :: State
initialState =
    { screen: Password
    , back: "/"
    , publishing: Nothing
    , post: Nothing
    , contacting: Nothing
    , emailOrNickname: ""
    , password: ""
    , nickname: ""
    , email: ""
    , errors: noErrors
    , sending: false
    }

steamRefused :: String
steamRefused = "Steam couldn't sign you in. Continue with Steam again."

isSignIn :: String -> Boolean
isSignIn path = path == "/signin" || isJust (stripPrefix (Pattern "/signin?") path)

-- The same checks the server makes, so most mistakes are named before sending.
validateRegistering :: Registering -> State -> Errors
validateRegistering registering { nickname, email } = noErrors
    { nickname = if null $ trim nickname then Just "Choose a nickname." else Nothing
    , email = case registering of
        WithSteam _ -> emailError email
        WithDiscord _ -> Nothing
    }

component :: ∀ query input output left. H.Component query input output (Async left)
component = Hooks.component \_ _ -> Hooks.do
    state /\ stateId <- Hooks.useState initialState

    let set = Hooks.modify_ stateId
        failWith errors = set _ { sending = false, errors = errors }

        showPassword back = do
            set _ { screen = Password, back = back, publishing = publishing back }
            void $ Hooks.fork do
                me <- H.lift fetchMe
                when (isJust me) $ navigateReplace_ back
            for_ (publishing back) \publishing' -> void $ Hooks.fork do
                post <- H.lift $ publishingPost publishing'
                set _ { post = post }
            for_ (contacting back) \contacting' -> void $ Hooks.fork do
                owner <- H.lift $ contactingOwner contacting'
                set _ { contacting = owner }

        -- A sign-in that fails on the way back leaves the player on the
        -- sign-in screen, whose buttons try again.
        returnFailed back error = do
            liftEffect $ replaceState {} (authPath "/signin" back)
            showPassword back
            set _ { errors = noErrors { returning = Just error } }

        -- `onUnknown` is given what to do with any other refusal.
        startSession provider back content onUnknown = do
            set _ { screen = Returning provider, back = back, publishing = publishing back }
            let failed = returnFailed back somethingWrong
            result <- H.lift $ Async.attempt $ fetchBody (expecting [ "badRequest" ] (Proxy :: _ StartSession)) content
            case result of
                Right response -> response # onMatch
                    { noContent: const $ navigateReplace_ back
                    , badRequest: onUnknown failed
                    }
                    (const failed)
                Left _ -> failed

        startDiscordSession accessToken back =
            startSession "Discord" back (inj (Proxy :: _ "discord") { accessToken }) \failed -> onMatch
                { unknownDiscord: \{ nickname } ->
                    set _ { screen = Nickname $ WithDiscord { accessToken }, nickname = nickname }
                }
                (const failed)

        startSteamSession assertion back =
            startSession "Steam" back (inj (Proxy :: _ "steam") { assertion }) \failed -> onMatch
                { unknownSteam: \{ nickname, ticket } ->
                    set _ { screen = Nickname $ WithSteam { ticket, offered: not $ null nickname }, nickname = nickname }
                , steamRefused: const $ returnFailed back steamRefused
                }
                (const failed)

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
                result <- H.lift $ Async.attempt $ fetchBody (expecting [ "badRequest" ] (Proxy :: _ StartSession))
                    (inj (Proxy :: _ "password")
                        { emailOrNickname: trim state.emailOrNickname, password: state.password })
                case result of
                    Right response -> response # onMatch
                        { noContent: const $ navigate_ state.back
                        , badRequest: onMatch
                            { unknownPlayer: const $ failWith noErrors
                                { emailOrNickname = Just "No account exists with this email or nickname." }
                            , wrongPassword: const $ failWith noErrors
                                { password = Just "Entered password is incorrect." }
                            }
                            (const $ failWith noErrors { form = Just somethingWrong })
                        }
                        (const $ failWith noErrors { form = Just somethingWrong })
                    Left _ -> failWith noErrors { form = Just somethingWrong }

        submitNickname registering event = do
            H.liftEffect $ preventDefault event
            let errors = validateRegistering registering state
            if isJust errors.nickname || isJust errors.email
            then failWith errors
            else do
                set _ { sending = true, errors = noErrors }
                result <- H.lift $ Async.attempt $ fetchBody (expecting [ "badRequest" ] (Proxy :: _ RegisterPlayer))
                    case registering of
                        WithDiscord { accessToken } ->
                            inj (Proxy :: _ "discord") { nickname: trim state.nickname, accessToken }
                        WithSteam { ticket } ->
                            inj (Proxy :: _ "steam") { nickname: trim state.nickname, email: trim state.email, ticket }
                case result of
                    Right response -> response # onMatch
                        { noContent: const $ navigateReplace_ state.back
                        , badRequest: match
                            { registration: \registrationErrors -> let
                                has error = elem error registrationErrors
                                in
                                failWith noErrors
                                    { nickname = if has (inj (Proxy :: _ "nickname") {}) then Just nicknameInvalid else Nothing
                                    , email = if has (inj (Proxy :: _ "email") {}) then Just emailInvalid else Nothing
                                    , form = if has (inj (Proxy :: _ "password") {}) then Just somethingWrong else Nothing
                                    }
                            , nicknameTaken: const $ failWith noErrors { nickname = Just nicknameTaken }
                            , discordTaken: const $ failWith noErrors
                                { form = Just "This Discord account already has a TeamTavern account. Sign in with Discord instead." }
                            , steamTaken: const $ failWith noErrors
                                { form = Just "This Steam account already has a TeamTavern account. Sign in with Steam instead." }
                            , steamTicket: const $ failWith noErrors
                                { form = Just "Steam's sign-in has run out. Go back and continue with Steam again." }
                            , emailTaken: const $ failWith noErrors { form = Just somethingWrong }
                            }
                        }
                        (const $ failWith noErrors { form = Just somethingWrong })
                    Left _ -> failWith noErrors { form = Just somethingWrong }

        -- Turned back at Discord or Steam: the page the trip set out from,
        -- which may be this one.
        turnedBack { from }
            | isSignIn from = do
                liftEffect $ replaceState {} from
                readBack >>= showPassword
            | otherwise = navigateReplace_ from

    Hooks.useLifecycleEffect do
        discordReturn <- takeDiscordReturn
        steamReturn <- takeSteamReturn
        case discordReturn, steamReturn of
            Just { accessToken: Just accessToken, trip: { back, switching: true } }, _ -> do
                keepSwitchToken accessToken
                navigateReplace_ back
            Just { accessToken: Just accessToken, trip: { back } }, _ ->
                void $ Hooks.fork $ startDiscordSession accessToken back
            Just { trip }, _ -> turnedBack trip
            _, Just { assertion: Just assertion, trip: { back, switching: true } } -> do
                keepSwitchAssertion assertion
                navigateReplace_ back
            _, Just { assertion: Just assertion, trip: { back } } -> void $ Hooks.fork $ startSteamSession assertion back
            _, Just { trip } -> turnedBack trip
            Nothing, Nothing -> readBack >>= showPassword
        pure Nothing

    let formError = case state.errors.form of
            Just error -> [ flowError error ]
            Nothing -> []

    Hooks.pure case state.screen of
        Password -> flow $
            [ HH.h1_ [ HH.text if isJust state.publishing then "Sign in to publish" else "Sign in" ] ]
            <> (case state.publishing, state.contacting of
                Just _, _ -> [ flowLead $ "Your " <> fromMaybe "post" state.post
                    <> " goes live as soon as you're signed in. Nothing you wrote is lost." ]
                _, Just owner -> [ flowLead $ "You'll come straight back to " <> owner <> "'s post." ]
                _, _ -> [])
            <> (case state.errors.returning of
                Just error -> [ flowError error ]
                Nothing -> [])
            <>
            [ button Outline Regular (authorizeWithDiscord state.back)
                [ Icons.discord, HH.text "Continue with Discord" ]
            , button Outline Regular (authorizeWithSteam state.back)
                [ Icons.steam, HH.text "Continue with Steam" ]
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
        Returning provider -> flow
            [ HH.h1_ [ HH.text "Sign in" ]
            , flowLead $ "Signing you in with " <> provider <> "…"
            ]
        Nickname registering -> flow
            [ HH.h1_ [ HH.text "Pick a nickname" ]
            , flowLead case registering of
                WithDiscord _ -> "It's shown on your posts and your messages. We took it from Discord; change it if you like."
                WithSteam { offered: true } -> "It's shown on your posts and your messages. We took it from Steam; change it if you like."
                WithSteam _ -> "It's shown on your posts and your messages."
            , formTight (submitNickname registering) $
                [ textField
                    { id: "signin-nickname", label: "Nickname"
                    , type_: HP.InputText, autocomplete: HP.AutocompleteNickname
                    , hint: Nothing, error: state.errors.nickname
                    , value: state.nickname, onInput: \value -> set _ { nickname = value }
                    }
                ]
                <> (case registering of
                    WithDiscord _ -> []
                    WithSteam _ ->
                        [ textField
                            { id: "signin-steam-email", label: "Email"
                            , type_: HP.InputEmail, autocomplete: HP.AutocompleteEmail
                            , hint: Just "Steam doesn't share it. Matches, messages and renewals are emailed here."
                            , error: state.errors.email
                            , value: state.email, onInput: \value -> set _ { email = value }
                            }
                        ])
                <> formError
                <> [ submitButton state.sending if isJust state.publishing then "Publish post" else "Continue" ]
            ]

signIn :: ∀ action slots left. H.ComponentHTML action (signIn :: Slot___ | slots) (Async left)
signIn = HH.slot_ (Proxy :: _ "signIn") unit component unit

module TeamTavern.Client.Pages.Oauth (oauth) where

import Prelude

import Async (Async, unify)
import Async as Async
import Data.Array (singleton)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (onMatch)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (lift)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Form (form, otherFormError)
import TeamTavern.Client.Components.Input (inputError, inputGroup, inputLabel_, requiredTextLineInputNamed)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Pages.Onboarding as Onboarding
import TeamTavern.Client.Script.Analytics (aliasNickname, identifyNickname, track_)
import TeamTavern.Client.Script.Navigate (hardNavigate, navigate, navigateWithEvent_)
import TeamTavern.Client.Script.QueryParams (getFragmentParam)
import TeamTavern.Client.Shared.Fetch (fetchBody)
import TeamTavern.Client.Shared.Slot (SimpleSlot)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Oauth.DiscordOauth (DiscordOauth)
import TeamTavern.Routes.Oauth.DiscordOauthExists (DiscordOauthExists)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type LoadedState =
    { nickname :: String
    , nicknameError :: Boolean
    , nicknameTaken :: Boolean
    , otherError :: Boolean
    , submitting :: Boolean
    , accessToken :: String
    }

data State
    = Empty
    | CheckError
    | Loaded LoadedState

render :: forall s m. MonadEffect m =>
    LoadedState -> (String -> m Unit) -> (Event -> m Unit) -> HH.HTML s (m Unit)
render {nickname, nicknameError, nicknameTaken, otherError, submitting} updateNickname submit' =
    form submit' $
    [ HH.h1 [ HS.class_ "form-heading" ]
        [ HH.text "Create your "
        , HH.a
            [ HP.href "/"
            , HE.onClick $ navigateWithEvent_ "/"
            ]
            [ HH.text "TeamTavern" ]
        , HH.text " account with Discord"
        ]
    , inputGroup $
        [ inputLabel_ "Nickname"
        , requiredTextLineInputNamed "nickname" nickname updateNickname
        ]
        <> inputError nicknameError """Nickname cannot be more than 40 characters long
            and can only contain alphanumeric characters, dashes, underscores and dots."""
        <> inputError nicknameTaken "This nickname is already taken, please pick another one."
    , HH.button
        [ HS.class_ "primary-button"
        , HP.disabled $ nickname == "" || submitting
        ]
        [ HH.i [ HS.class_ "fas fa-user-check button-icon" ] []
        , HH.text $
            if submitting
            then "Creating account..."
            else "Create account"
        ]
    ]
    <> otherFormError otherError

sendRegisterRequest :: ∀ left. LoadedState -> Async left (Maybe LoadedState)
sendRegisterRequest state @ {nickname, accessToken} = Async.unify do
    response <- fetchBody (Proxy :: _ DiscordOauth) {nickname, accessToken}
        # lmap (const $ Just $ state {otherError = true})
    pure $ onMatch {noContent: const Nothing} (const $ Just $ state {otherError = true}) response

submit :: forall left.
    LoadedState -> Hooks.StateId State -> Event -> Hooks.HookM (Async left) Unit
submit state stateId event = do
    liftEffect $ preventDefault event
    newStateMaybe <- lift $ sendRegisterRequest state
    case newStateMaybe of
        Nothing -> do
            aliasNickname
            identifyNickname
            track_ "Register Discord"
            navigate Onboarding.emptyInput "/onboarding/start"
        Just newState -> Hooks.put stateId $ Loaded newState { submitting = false }

sendCheckRequest :: forall left.
    String -> Async left (Maybe {exists :: Boolean})
sendCheckRequest accessToken = unify do
    response <- fetchBody (Proxy :: _ DiscordOauthExists) {accessToken}
        # lmap (const Nothing)
    pure $ onMatch {ok: Just} (const Nothing) response

component :: forall q i o left. H.Component q i o (Async left)
component = Hooks.component \_ _ -> Hooks.do
    state /\ stateId <- Hooks.useState Empty
    Hooks.useLifecycleEffect $ Nothing <$ do
        accessTokenMaybe <- getFragmentParam "access_token"
        case accessTokenMaybe of
            Nothing -> hardNavigate "/"
            Just accessToken -> do
                result <- lift $ sendCheckRequest accessToken
                case result of
                    Nothing -> Hooks.put stateId CheckError
                    Just {exists} | exists -> hardNavigate "/"
                    Just _ ->
                        Hooks.put stateId $ Loaded
                            { nickname: ""
                            , nicknameError: false
                            , nicknameTaken: false
                            , otherError: false
                            , submitting: false
                            , accessToken
                            }
    Hooks.pure case state of
        Empty -> HH.div_ []
        CheckError -> HH.div [HS.class_ "single-message"]
            [ HH.p_ [HH.text "Something unexpected went wrong! Please try again later."]
            , navigationAnchor (Proxy :: _ "homeAnchor")
                {path: "/", content: HH.text "Back to home page"}
            ]
        Loaded loaded ->
            HH.div [ HS.class_ "single-form-container" ] $ singleton $
            render
            loaded
            (\nickname -> Hooks.put stateId $ Loaded loaded {nickname = nickname})
            (submit loaded stateId)

oauth :: ∀ query children left.
    HH.ComponentHTML query (register :: SimpleSlot | children) (Async left)
oauth = HH.slot (Proxy :: _ "register") unit component unit absurd

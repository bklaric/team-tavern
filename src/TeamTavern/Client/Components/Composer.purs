module TeamTavern.Client.Components.Composer
    ( ComposerActions
    , ComposerState
    , UseComposer
    , composer
    , idleComposer
    , useComposer
    ) where

import Prelude

import Data.Foldable (traverse_)
import Data.String (trim)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Hooks (type (<>), Hook, HookM, HookType, Pure, UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Hook (class HookNewtype)
import TeamTavern.Client.Components.UsePhone (UsePhone, usePhone)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Script.Thread (autosize)
import TeamTavern.Client.Snippets.Class as HS
import Web.Event.Event (Event, preventDefault)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, isComposing, key, shiftKey)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

-- The message box under a conversation (brief 5.6), in the contact panel and
-- in the inbox.

-- | `unsent` is whether the last message couldn't be sent, which is then
-- | still in the box.
type ComposerState = { draft :: String, sending :: Boolean, unsent :: Boolean }

type ComposerActions i = { onDraft :: String -> i, onKeyDown :: KeyboardEvent -> i, onSend :: Event -> i }

idleComposer :: ComposerState
idleComposer = { draft: "", sending: false, unsent: false }

-- | The box and Send, filled when the message box is the way the post asks to
-- | be reached and outlined when it comes second, and under them what went
-- | wrong sending. The box takes 2000 characters, as the server does.
composer :: ∀ w i.
    { ref :: H.RefLabel, primary :: Boolean, state :: ComposerState, actions :: ComposerActions i }
    -> Array (HH.HTML w i)
composer { ref, primary, state, actions } =
    [ HH.form [ HS.class_ "composer", HE.onSubmit actions.onSend ]
        [ HH.textarea
            [ HS.class_ "textarea"
            , HP.ref ref
            , HP.rows 1
            , HPA.label "Message"
            , HP.placeholder "Write a message…"
            , HP.attr (HH.AttrName "maxlength") "2000"
            , HP.value state.draft
            , HE.onValueInput actions.onDraft
            , HE.onKeyDown actions.onKeyDown
            ]
        , HH.button
            [ HS.class_ if primary then "button button-primary" else "button button-outline"
            , HP.type_ HP.ButtonSubmit
            , HP.disabled state.sending
            ]
            [ HH.text "Send" ]
        ]
    ]
    <> if state.unsent
        then [ HH.span [ HS.class_ "field-error", HP.attr (HH.AttrName "role") "alert" ]
            [ Icons.circleAlert, HH.text "Your message couldn't be sent. Try again." ] ]
        else []

foreign import data UseComposer :: HookType

instance HookNewtype UseComposer (UsePhone <> UseState ComposerState <> Pure)

-- | The box's draft and its sending, given how to send a message, which
-- | answers whether it went. Enter sends and Shift+Enter starts a new line, as
-- | on Discord; on a phone Enter is a new line and Send sends.
useComposer :: ∀ m. MonadEffect m =>
    H.RefLabel -> (String -> HookM m Boolean)
    -> Hook m UseComposer { state :: ComposerState, actions :: ComposerActions (HookM m Unit), clear :: HookM m Unit }
useComposer ref send = Hooks.wrap Hooks.do
    phone <- usePhone
    state /\ stateId <- Hooks.useState idleComposer

    let fit = Hooks.getHTMLElementRef ref >>= traverse_ (liftEffect <<< autosize)

        submit = do
            { draft, sending } <- Hooks.get stateId
            when (not sending && trim draft /= "") do
                Hooks.modify_ stateId _ { sending = true, unsent = false }
                sent <- send draft
                -- What was written while it went stays.
                Hooks.modify_ stateId \state' ->
                    if sent
                    then state' { draft = if state'.draft == draft then "" else state'.draft, sending = false }
                    else state' { sending = false, unsent = true }
                fit

        actions =
            { onDraft: \draft -> do
                Hooks.modify_ stateId _ { draft = draft }
                fit
            , onKeyDown: \event ->
                when (key event == "Enter" && not (shiftKey event) && not (isComposing event) && not phone) do
                    liftEffect $ preventDefault $ KeyboardEvent.toEvent event
                    submit
            , onSend: \event -> do
                liftEffect $ preventDefault event
                submit
            }

        clear = do
            Hooks.put stateId idleComposer
            fit

    Hooks.pure { state, actions, clear }

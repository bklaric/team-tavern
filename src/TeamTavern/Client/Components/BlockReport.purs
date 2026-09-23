module TeamTavern.Client.Components.BlockReport
    ( Actions
    , BlockReport
    , ReportForm
    , Subject
    , UseBlockReport
    , View(..)
    , blockReportBody
    , moreMenu
    , useBlockReport
    ) where

import Prelude

import Async (Async)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Hooks (type (<>), Hook, HookM, HookType, Pure, UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Hook (class HookNewtype)
import TeamTavern.Client.Components.Button (Size(..), Weight(..), button)
import TeamTavern.Client.Components.Check (check, choiceList)
import TeamTavern.Client.Components.Confirm (confirm)
import TeamTavern.Client.Components.Field (Labelling(..), field, field_)
import TeamTavern.Client.Components.Flow (flowError, submitButton)
import TeamTavern.Client.Components.Input (textarea)
import TeamTavern.Client.Components.Menu (menuItem, menuItemDestructive)
import TeamTavern.Client.Components.Overlay (Presentation(..), UseOverlay, overlay, useOverlay)
import TeamTavern.Client.Components.Toast (Toast)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Script.Focus (focusSoon)
import TeamTavern.Client.Script.Unread (announceUnread)
import TeamTavern.Client.Shared.Block (block, unblock)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Shared.Report (Report, reasons)
import Web.Event.Event (Event, preventDefault)

-- The ⋯ menu of the contact panel and of a conversation, with Report and
-- Block, and what each opens in place of the panel's or the conversation's
-- body: the confirmation saying what a block does, and the report form
-- (brief 10).

type ReportForm =
    { reason :: String
    , detail :: String
    , alsoBlock :: Boolean
    , missing :: Boolean
    , sending :: Boolean
    , failed :: Boolean
    }

data View = Main | Blocking | Reporting ReportForm

-- | Who a block or a report is against, and how the report goes: about the
-- | post from its panel, about the conversation from the inbox.
type Subject left = { who :: String, report :: Report -> Async left Boolean }

type Actions i =
    { onMenu :: i
    , onCloseMenu :: i
    , onAskReport :: i
    , onAskBlock :: i
    , onCancel :: i
    , onBlock :: i
    , onReason :: String -> i
    , onDetail :: String -> i
    , onAlsoBlock :: Boolean -> i
    , onSend :: Event -> i
    }

-- | The menu and the view, and what they do, which the panel or the
-- | conversation draws.
type BlockReport i = { menuOpen :: Boolean, view :: View, actions :: Actions i }

type State = { menuOpen :: Boolean, view :: View }

initial :: State
initial = { menuOpen: false, view: Main }

newForm :: ReportForm
newForm = { reason: "", detail: "", alsoBlock: false, missing: false, sending: false, failed: false }

menuPresentation :: Presentation
menuPresentation = Dropdown { className: "menu", role: "menu" }

blockText :: String
blockText = "You won't see each other's posts, your conversations leave both inboxes, and neither of you hears about the other's new posts. Nothing is deleted: unblocking brings it all back."

-- | The ⋯ button, and the menu it opens under itself.
moreMenu :: ∀ w i. H.RefLabel -> { who :: String, reportLabel :: String } -> BlockReport i -> HH.HTML w i
moreMenu ref { who, reportLabel } { menuOpen, actions } =
    HH.div [ HS.class_ "menu-wrap" ]
    [ HH.button
        [ HS.class_ "icon-button"
        , HP.type_ HP.ButtonButton
        , HPA.label "More"
        , HPA.hasPopup "menu"
        , HPA.expanded $ show menuOpen
        , HE.onClick $ const actions.onMenu
        ]
        [ Icons.ellipsis ]
    , if menuOpen
        then overlay { ref, presentation: menuPresentation, title: "More", onClose: actions.onCloseMenu }
            [ menuItem actions.onAskReport [ Icons.flag, HH.text reportLabel ]
            , menuItemDestructive actions.onAskBlock [ Icons.ban, HH.text $ "Block " <> who ]
            ]
            []
        else HH.text ""
    ]

-- | What stands in place of the body while the player blocks or reports, or
-- | nothing, when the body shows.
blockReportBody :: ∀ w i. { who :: String, reportTitle :: String } -> BlockReport i -> Maybe (HH.HTML w i)
blockReportBody { who, reportTitle } { view, actions } = case view of
    Main -> Nothing
    Blocking -> Just $ confirm
        { id: "block"
        , title: "Block " <> who <> "?"
        , text: blockText
        , action: [ Icons.ban, HH.text $ "Block " <> who ]
        , onConfirm: actions.onBlock
        , cancel: "Cancel"
        , onCancel: actions.onCancel
        }
    Reporting form -> Just $
        HH.form [ HS.class_ "form form-report", HP.noValidate true, HE.onSubmit actions.onSend ] $
        [ HH.div [ HS.class_ "panel-section" ]
            [ HH.h3_ [ HH.text $ "Report " <> reportTitle ]
            , HH.p [ HS.class_ "muted" ] [ HH.text "Reports go to the people who run TeamTavern." ]
            ]
        , field
            ((field_ "report-reason" "What's wrong?")
                { labelling = Group, error = if form.missing then Just "Choose what's wrong." else Nothing })
            [ choiceList { id: "report-reason", options: reasons, chosen: form.reason, onChoose: actions.onReason } ]
        , field (field_ "report-detail" "Anything we should know?")
            [ textarea [ HP.id "report-detail", HP.rows 3, HP.attr (HH.AttrName "maxlength") "2000" ]
                { value: form.detail, placeholder: "", onInput: actions.onDetail }
            ]
        , check { id: "report-block", text: "Also block " <> who, checked: form.alsoBlock, onChange: actions.onAlsoBlock }
        ]
        <> (if form.failed then [ flowError "The report couldn't be sent. Try again." ] else [])
        <>
        [ HH.div [ HS.class_ "confirm-actions" ]
            [ submitButton form.sending "Send report"
            , button Text Regular actions.onCancel [ HH.text "Cancel" ]
            ]
        ]

foreign import data UseBlockReport :: HookType

instance HookNewtype UseBlockReport (UseState State <> UseOverlay <> Pure)

-- | Block and report for whatever `subject` answers with when they are taken.
-- | `scope` is a selector for the element holding the menu, so the focus
-- | lands in the right place. A block closes what it was taken from with
-- | `close`, and it and its Undo tell the page with `changed`, which asks
-- | again for what the block hides. `back` returns to the body, answering
-- | whether there was anywhere to return from, for Escape; `reset` does so
-- | for a panel or conversation opened anew.
useBlockReport :: ∀ left.
    { ref :: H.RefLabel
    , scope :: String
    , subject :: HookM (Async left) (Maybe (Subject left))
    , close :: HookM (Async left) Unit
    , changed :: HookM (Async left) Unit
    , showToast :: Toast (Async left) -> HookM (Async left) Unit
    }
    -> Hook (Async left) UseBlockReport
        { blockReport :: BlockReport (HookM (Async left) Unit)
        , back :: HookM (Async left) Boolean
        , reset :: HookM (Async left) Unit
        }
useBlockReport { ref, scope, subject, close, changed, showToast } = Hooks.wrap Hooks.do
    state /\ stateId <- Hooks.useState initial

    let closeMenu = Hooks.modify_ stateId _ { menuOpen = false }

    useOverlay ref menuPresentation state.menuOpen closeMenu

    let focus selector = liftEffect $ focusSoon $ scope <> " " <> selector
        reset = Hooks.put stateId initial
        toMain = do
            reset
            focus "[aria-label=More]"
        back = Hooks.get stateId >>= case _ of
            { view: Main } -> pure false
            _ -> toMain $> true
        setForm f = Hooks.modify_ stateId \state' -> case state'.view of
            Reporting form -> state' { view = Reporting $ f form }
            _ -> state'

        undo who = do
            unblocked <- H.lift $ unblock who
            if unblocked
            then do
                changed
                liftEffect announceUnread
                showToast { text: who <> " is unblocked.", action: Nothing }
            else showToast { text: who <> " couldn't be unblocked. Try again.", action: Nothing }

        -- What was blocked from goes, and the page asks again for what is left.
        blocked who text = do
            reset
            close
            changed
            liftEffect announceUnread
            showToast { text, action: Just { label: "Undo", onAction: undo who } }

        askBlock = do
            Hooks.put stateId { menuOpen: false, view: Blocking }
            focus "#block-title ~ .confirm-actions .button-text"
        askReport = do
            Hooks.put stateId { menuOpen: false, view: Reporting newForm }
            focus "[name=report-reason]"

        confirmBlock = subject >>= case _ of
            Nothing -> pure unit
            Just { who } -> do
                done <- H.lift $ block who
                if done
                then blocked who $ who <> " is blocked."
                else showToast { text: who <> " couldn't be blocked. Try again.", action: Nothing }

        send event = do
            liftEffect $ preventDefault event
            current <- Hooks.get stateId
            subject' <- subject
            case current.view, subject' of
                Reporting form, Just { who, report } | not form.sending ->
                    if form.reason == ""
                    then do
                        setForm _ { missing = true }
                        focus "[name=report-reason]"
                    else do
                        setForm _ { sending = true, failed = false }
                        sent <- H.lift $ report { reason: form.reason, detail: form.detail, block: form.alsoBlock }
                        case sent, form.alsoBlock of
                            true, true -> blocked who $ "Report sent. " <> who <> " is blocked."
                            true, false -> do
                                toMain
                                showToast { text: "Report sent. Thanks for telling us.", action: Nothing }
                            false, _ -> setForm _ { sending = false, failed = true }
                _, _ -> pure unit

        blockReport =
            { menuOpen: state.menuOpen
            , view: state.view
            , actions:
                { onMenu: Hooks.modify_ stateId \state' -> state' { menuOpen = not state'.menuOpen }
                , onCloseMenu: closeMenu
                , onAskReport: askReport
                , onAskBlock: askBlock
                , onCancel: toMain
                , onBlock: confirmBlock
                , onReason: \reason -> setForm _ { reason = reason, missing = false }
                , onDetail: \detail -> setForm _ { detail = detail }
                , onAlsoBlock: \alsoBlock -> setForm _ { alsoBlock = alsoBlock }
                , onSend: send
                }
            }

    Hooks.pure { blockReport, back, reset }

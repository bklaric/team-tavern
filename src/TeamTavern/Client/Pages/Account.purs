module TeamTavern.Client.Pages.Account (account) where

import Prelude

import Async (Async)
import Async as Async
import Control.Parallel (parallel, sequential)
import Data.Array (catMaybes, elem, filter, mapMaybe, null, snoc, sortWith, unsnoc)
import Data.Date (Date)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (joinWith, toLower, trim)
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (match, onMatch)
import Effect.Class (liftEffect)
import Effect.Now (nowDate)
import Foreign.Object (Object)
import Foreign.Object as Object
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Button (Size(..), Weight(..), button)
import TeamTavern.Client.Components.Check (switch, switches)
import TeamTavern.Client.Components.Confirm (confirm)
import TeamTavern.Client.Components.DataList (dataList, personRow, personRows)
import TeamTavern.Client.Components.Field (Labelling(..), field, field_, formSection)
import TeamTavern.Client.Components.Flow (flowError)
import TeamTavern.Client.Components.Input (input, select)
import TeamTavern.Client.Components.Toast (toasts, useToast)
import TeamTavern.Client.Components.Tokens as Tokens
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Script.Back (authPath)
import TeamTavern.Client.Script.Focus (focusSoon)
import TeamTavern.Client.Script.Navigate (navigateReplace_, navigate_)
import TeamTavern.Client.Script.Scroll (focusCentered, focusFirstInvalid)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Script.Unread (announceUnread)
import TeamTavern.Client.Shared.AccountErrors (nicknameInvalid, nicknameTaken, somethingWrong)
import TeamTavern.Client.Shared.Block (block, unblock)
import TeamTavern.Client.Shared.Contacts (contactLabel, contactPlaceholder)
import TeamTavern.Client.Shared.Facts (ageOn, dateText, timezoneOptions, timezoneText)
import TeamTavern.Client.Shared.Fetch (fetchBody, fetchSimple)
import TeamTavern.Client.Shared.Slot (Slot__I)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Account.DeleteAccount (DeleteAccount)
import TeamTavern.Routes.Account.UpdateFacts (UpdateFacts)
import TeamTavern.Routes.Account.UpdateFacts as UpdateFacts
import TeamTavern.Routes.Account.UpdateSwitches (UpdateSwitches)
import TeamTavern.Routes.Account.ViewAccount (ViewAccount)
import TeamTavern.Routes.Account.ViewAccount as ViewAccount
import TeamTavern.Routes.Block.ViewBlocked (ViewBlocked)
import TeamTavern.Routes.Country.ViewCountries (ViewCountries)
import TeamTavern.Shared.Languages (allLanguages)
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event, preventDefault)
import Web.HTML (window)
import Web.HTML.Location as Location
import Web.HTML.Window (location)

-- The account page (brief 11.5): the facts and contacts every post of the
-- player's shows, edited here once for all of them, and the parts of the
-- account nobody else sees.

type Loaded =
    { account :: ViewAccount.OkContent
    , blocked :: Array String
    , countries :: Array String
    , today :: Date
    }

data Page = Loading | SignedOut | Failed | Loaded Loaded

-- The facts form as the player has it, every value as its control holds it.
type Facts =
    { nickname :: String
    , birthday :: String
    , country :: String
    , languages :: Array String
    , timezone :: String
    , contacts :: Object String
    }

-- `errors` are keyed by the field they are under, and `form` is what went
-- wrong with the form as a whole.
type State =
    { page :: Page
    , facts :: Maybe Facts
    , errors :: Object String
    , saving :: Boolean
    , confirmingDelete :: Boolean
    }

initialState :: State
initialState =
    { page: Loading, facts: Nothing, errors: Object.empty, saving: false, confirmingDelete: false }

-- The rows a link to the page can land on, by the fragment it names.
landings :: Array String
landings = [ "email", "emails", "blocked" ]

load :: ∀ left. Async left Page
load = do
    results <- sequential $ { account: _, blocked: _, countries: _ }
        <$> parallel (Async.attempt $ fetchSimple (Proxy :: _ ViewAccount))
        <*> parallel (Async.attempt $ fetchSimple (Proxy :: _ ViewBlocked))
        <*> parallel (Async.attempt $ fetchSimple (Proxy :: _ ViewCountries))
    today <- liftEffect nowDate
    pure case hush results.account of
        Nothing -> Failed
        Just response -> response # onMatch
            { ok: \account' -> fromMaybe Failed do
                blocked <- hush results.blocked >>= onMatch { ok: Just } (const Nothing)
                countries <- hush results.countries >>= onMatch { ok: Just } (const Nothing)
                pure $ Loaded
                    { account: account'
                    , blocked: blocked <#> _.nickname
                    , countries: countries.countries <#> _.name
                    , today
                    }
            , notAuthorized: const SignedOut
            }
            (const Failed)

blank :: String -> Maybe String
blank value = if trim value == "" then Nothing else Just value

factsOf :: String -> ViewAccount.OkContent -> Facts
factsOf clientTimezone account' =
    { nickname: account'.nickname
    , birthday: fromMaybe "" account'.birthday
    , country: fromMaybe "" account'.country
    , languages: account'.languages
    , timezone: fromMaybe clientTimezone account'.timezone
    , contacts: Object.fromFoldable $ account'.contacts # mapMaybe \{ kind, value } -> value <#> Tuple kind
    }

factsRequest :: Facts -> UpdateFacts.RequestContent
factsRequest facts =
    { nickname: facts.nickname
    , account:
        { country: blank facts.country
        , languages: facts.languages
        , birthday: blank facts.birthday
        , timezone: blank facts.timezone
        , contacts: facts.contacts
        }
    }

-- What the page can tell before asking the server.
factsErrors :: Date -> Facts -> Object String
factsErrors today facts = Object.fromFoldable $ catMaybes
    [ if trim facts.nickname == "" then Just $ Tuple "nickname" "Choose a nickname." else Nothing
    , if facts.birthday /= "" && not (isJust $ ageOn today facts.birthday)
        then Just $ Tuple "birthday" birthdayInvalid
        else Nothing
    ]

birthdayInvalid :: String
birthdayInvalid = "Enter the day you were born."

serverErrors :: UpdateFacts.BadContent -> Object String
serverErrors = match
    { facts: \errors -> Object.fromFoldable $ errors <#> match
        { nickname: \_ -> Tuple "nickname" nicknameInvalid
        , contact: \{ kind } -> Tuple kind $
            "Use up to " <> (if kind == "discord" then "37" else "100") <> " characters."
        , field: \{ key } -> Tuple key if key == "birthday" then birthdayInvalid else somethingWrong
        }
    , nicknameTaken: \_ -> Object.singleton "nickname" nicknameTaken
    }

-- How many posts a change here reaches. A player without posts is told what
-- the facts are for rather than counted at.
factsLead :: Int -> String
factsLead 0 = "Every post you write shows these, as your account has them then. You only give them once."
factsLead posts =
    "Every post you have shows these, as your account has them now. Change one here and it changes on "
    <> (if posts == 1 then "your post" else "all " <> show posts <> " of your posts")
    <> " at once."

savedText :: Int -> String
savedText 0 = "Saved."
savedText 1 = "Saved. Your post shows it."
savedText posts = "Saved. All " <> show posts <> " of your posts show it."

counted :: Int -> String -> Maybe String
counted 0 _ = Nothing
counted 1 one = Just $ "1 " <> one
counted count one = Just $ show count <> " " <> one <> "s"

deleteText :: ViewAccount.OkContent -> String
deleteText { posts, conversations } =
    case catMaybes [ counted posts "post", counted conversations "conversation" ] of
        [] -> "There is nothing on it to lose, and this can't be undone."
        counts -> "Your " <> joinWith " and " counts
            <> (if posts + conversations == 1 then " goes" else " go") <> " with it"
            <> (if conversations > 0 then ", for the players you were talking to as well" else "")
            <> ". This can't be undone."

-- "Valorant and League of Legends", "Apex Legends, Valheim and Overwatch".
listed :: Array String -> String
listed items = case unsnoc items of
    Just { init, last } | not (null init) -> joinWith ", " init <> " and " <> last
    _ -> joinWith "" items

contactHint :: ViewAccount.Contact -> String
contactHint { everyGame, games } =
    if everyGame then "On all your posts." else "On your " <> listed games <> " posts."

hint :: ∀ w i. String -> HH.HTML w i
hint text = HH.p [ HS.class_ "field-hint" ] [ HH.text text ]

muted :: ∀ w i. String -> HH.HTML w i
muted text = HH.span [ HS.class_ "muted" ] [ HH.text text ]

given :: ∀ w i. Maybe String -> Array (HH.HTML w i)
given = maybe [ muted "Not given" ] \value -> [ HH.text value ]

component :: ∀ query output left. H.Component query Unit output (Async left)
component = Hooks.component \_ _ -> Hooks.do
    state /\ stateId <- Hooks.useState initialState
    { toast, showToast, dismissToast } <- useToast

    let modifyLoaded :: (Loaded -> Loaded) -> HookM (Async left) Unit
        modifyLoaded change = Hooks.modify_ stateId \state' -> state'
            { page = case state'.page of
                Loaded loaded -> Loaded $ change loaded
                page -> page
            }

        reload = do
            page <- H.lift load
            Hooks.modify_ stateId _ { page = page }

    Hooks.useLifecycleEffect do
        void $ Hooks.fork do
            page <- H.lift load
            hash <- liftEffect $ window >>= location >>= Location.hash
            case page of
                -- There is no account to show, so the page asks the player to
                -- sign in and comes back here, to the row it was opened on.
                SignedOut -> navigateReplace_ $ authPath "/signin" $ "/account" <> hash
                _ -> do
                    Hooks.modify_ stateId _ { page = page }
                    let landing = CodeUnits.drop 1 hash
                    when (elem landing landings) $ focusCentered landing
        pure Nothing

    let focusEdit = liftEffect $ focusSoon "#facts-title ~ .account-actions .button"

        startEditing = Hooks.get stateId >>= _.page >>> case _ of
            Loaded { account: account' } -> do
                clientTimezone <- getClientTimezone
                Hooks.modify_ stateId _ { facts = Just $ factsOf clientTimezone account', errors = Object.empty }
                liftEffect $ focusSoon "#account-nickname"
            _ -> pure unit

        cancelEditing = do
            Hooks.modify_ stateId _ { facts = Nothing, errors = Object.empty }
            focusEdit

        setFacts key change = Hooks.modify_ stateId \state' -> state'
            { facts = state'.facts <#> change, errors = Object.delete key state'.errors }

        saveFacts :: Event -> HookM (Async left) Unit
        saveFacts event = do
            liftEffect $ preventDefault event
            state' <- Hooks.get stateId
            case state'.page, state'.facts of
                Loaded loaded, Just facts | not state'.saving -> do
                    let errors = factsErrors loaded.today facts
                        failed errors' = do
                            Hooks.modify_ stateId _ { saving = false, errors = errors' }
                            liftEffect focusFirstInvalid
                    if not $ Object.isEmpty errors
                    then failed errors
                    else do
                        Hooks.modify_ stateId _ { saving = true, errors = Object.empty }
                        result <- H.lift $ Async.attempt $ fetchBody (Proxy :: _ UpdateFacts) (factsRequest facts)
                        case hush result of
                            Nothing -> failed $ Object.singleton "form" somethingWrong
                            Just response -> response # onMatch
                                { noContent: \_ -> do
                                    reload
                                    Hooks.modify_ stateId _ { facts = Nothing, saving = false }
                                    -- The header names the player.
                                    liftEffect announceUnread
                                    focusEdit
                                    showToast { text: savedText loaded.account.posts, action: Nothing }
                                , badRequest: failed <<< serverErrors
                                }
                                (\_ -> failed $ Object.singleton "form" somethingWrong)
                _, _ -> pure unit

        flipSwitch :: (ViewAccount.Switches -> ViewAccount.Switches) -> HookM (Async left) Unit
        flipSwitch change = Hooks.get stateId >>= _.page >>> case _ of
            Loaded { account: account' } -> do
                let before = account'.switches
                    after = change before
                    setSwitches switches' = modifyLoaded _ { account { switches = switches' } }
                setSwitches after
                result <- H.lift $ Async.attempt $ fetchBody (Proxy :: _ UpdateSwitches) after
                unless (isJust $ hush result >>= onMatch { noContent: const $ Just unit } (const Nothing)) do
                    setSwitches before
                    showToast { text: "Your emails couldn't be changed. Try again.", action: Nothing }
            _ -> pure unit

        reblock who = do
            blocked <- H.lift $ block who
            if blocked
            then do
                modifyLoaded \loaded -> loaded { blocked = loaded.blocked `snoc` who # sortWith toLower }
                liftEffect announceUnread
            else showToast { text: who <> " couldn't be blocked again. Try again.", action: Nothing }

        unblock' who = do
            unblocked <- H.lift $ unblock who
            if unblocked
            then do
                modifyLoaded \loaded -> loaded { blocked = loaded.blocked # filter (_ /= who) }
                liftEffect announceUnread
                liftEffect $ focusSoon "#blocked"
                showToast
                    { text: who <> " is unblocked. Their posts, and anything you wrote to each other, are back."
                    , action: Just { label: "Undo", onAction: reblock who }
                    }
            else showToast { text: who <> " couldn't be unblocked. Try again.", action: Nothing }

        askDelete = do
            Hooks.modify_ stateId _ { confirmingDelete = true }
            liftEffect $ focusSoon ".account .confirm .button-text"

        keepAccount = do
            Hooks.modify_ stateId _ { confirmingDelete = false }
            liftEffect $ focusSoon ".account-actions-end .button"

        deleteAccount = do
            result <- H.lift $ Async.attempt $ fetchSimple (Proxy :: _ DeleteAccount)
            if isJust $ hush result >>= onMatch { noContent: const $ Just unit } (const Nothing)
            then navigate_ "/?account=deleted"
            else showToast { text: "Your account couldn't be deleted. Try again.", action: Nothing }

    let errorOf key = Object.lookup key state.errors

        textInput id key label hint' value placeholder onInput =
            field ((field_ id label) { hint = hint', error = errorOf key })
            [ input [ HP.id id ] { value, placeholder, onInput } ]

        factsForm loaded facts =
            HH.form [ HS.class_ "form", HP.noValidate true, HE.onSubmit saveFacts ] $
            [ formSection Nothing
                [ textInput "account-nickname" "nickname" "Nickname" (Just "Shown on your posts and in your messages.")
                    facts.nickname "" \value -> setFacts "nickname" _ { nickname = value }
                , field ((field_ "account-birthday" "Birthday")
                    { hint = Just "Your posts show your age, never your birthday.", error = errorOf "birthday" })
                    [ input [ HP.id "account-birthday", HP.type_ HP.InputDate ]
                        { value: facts.birthday
                        , placeholder: ""
                        , onInput: \value -> setFacts "birthday" _ { birthday = value }
                        }
                    ]
                , field ((field_ "account-location" "Location") { error = errorOf "location" })
                    [ select [ HP.id "account-location" ]
                        { options: loaded.countries <#> \country -> { value: country, label: country }
                        , value: facts.country
                        , placeholder: Just "Choose a country"
                        , onChange: \value -> setFacts "location" _ { country = value }
                        }
                    ]
                , field ((field_ "account-languages" "Languages") { labelling = Group, error = errorOf "languages" })
                    [ Tokens.tokens
                        { id: "account-languages"
                        , one: "language"
                        , options: allLanguages <#> \language -> { value: language, label: language }
                        , chosen: facts.languages
                        }
                        \languages -> setFacts "languages" _ { languages = languages }
                    ]
                , field ((field_ "account-timezone" "Timezone")
                    { hint = Just "Your online hours are written in this timezone. Everyone else sees them in theirs."
                    , error = errorOf "timezone"
                    })
                    [ select [ HP.id "account-timezone" ]
                        { options: timezoneOptions
                        , value: facts.timezone
                        , placeholder: Nothing
                        , onChange: \value -> setFacts "timezone" _ { timezone = value }
                        }
                    ]
                ]
            , formSection (Just "Contacts") $
                [ hint "Every post offers messages on TeamTavern, so these are optional." ]
                <> (loaded.account.contacts <#> \contact@{ kind } ->
                    textInput ("account-contact-" <> kind) kind (contactLabel kind) (Just $ contactHint contact)
                        (Object.lookup kind facts.contacts # fromMaybe "") (contactPlaceholder kind)
                        \value -> setFacts kind \facts' -> facts' { contacts = Object.insert kind value facts'.contacts })
            ]
            <> maybe [] (\error -> [ flowError error ]) (errorOf "form")
            <>
            [ HH.div [ HS.class_ "account-actions" ]
                [ HH.button
                    [ HS.class_ "button button-primary", HP.type_ HP.ButtonSubmit, HP.disabled state.saving ]
                    [ Icons.check, HH.text "Save changes" ]
                , button Text Regular cancelEditing [ HH.text "Cancel" ]
                ]
            ]

        contactsText contacts =
            contacts # mapMaybe (\{ kind, value } -> value <#> \value' -> contactLabel kind <> " " <> value')
            # joinWith " · "

        factRows { account: account', today } = dataList
            [ { id: Nothing, label: "Nickname", value: [ HH.text account'.nickname ], action: Nothing }
            , { id: Nothing
              , label: "Birthday"
              , value: given $ account'.birthday <#> \birthday ->
                    dateText birthday <> maybe "" (\age -> ", shown as age " <> show age) (ageOn today birthday)
              , action: Nothing
              }
            , { id: Nothing, label: "Location", value: given account'.country, action: Nothing }
            , { id: Nothing
              , label: "Languages"
              , value: given if null account'.languages then Nothing else Just $ joinWith ", " account'.languages
              , action: Nothing
              }
            , { id: Nothing, label: "Timezone", value: given $ account'.timezone <#> timezoneText, action: Nothing }
            , { id: Nothing, label: "Contacts", value: given $ blank $ contactsText account'.contacts, action: Nothing }
            ]

        factsSection loaded =
            HH.section [ HS.class_ "account-section", HPA.labelledBy "facts-title" ] $
            [ HH.h2 [ HP.id "facts-title" ] [ HH.text "Shown on your posts" ]
            , HH.p [ HS.class_ "account-lead" ] [ HH.text $ factsLead loaded.account.posts ]
            ]
            <> case state.facts of
                Just facts -> [ factsForm loaded facts ]
                Nothing ->
                    [ factRows loaded
                    , HH.div [ HS.class_ "account-actions" ]
                        [ button Outline Small startEditing [ Icons.pencil, HH.text "Edit" ] ]
                    ]

        -- The address, and whether the site can use it yet: until its link is
        -- clicked the link is all it gets, and without one nothing is emailed.
        emailValue account' = case account'.email of
            Nothing ->
                [ muted "No address"
                , hint "Matches, messages and renewals are emailed, so without one the site can't tell you about them."
                ]
            Just email | not account'.emailConfirmed ->
                [ HH.text email
                , hint "Not confirmed yet. We sent it a link, and send it nothing else until the link is clicked."
                ]
            Just email -> [ HH.text email ]

        signInValue account' =
            if account'.signIn == "discord"
            then [ HH.span [ HS.class_ "data-line" ] [ Icons.discord, HH.text "Discord" ] ]
            else [ HH.text "Email and password" ]

        emailsValue account' = let
            switch' key text note checked change =
                switch { id: "account-email-" <> key, text, note: Just note, checked, onChange: flipSwitch <<< change }
            held = case account'.email of
                Nothing -> Just "None of these is sent without an address."
                Just _ | not account'.emailConfirmed -> Just "None of these is sent until your address is confirmed."
                Just _ -> Nothing
            in
            [ switches
                [ switch' "matches" "Matches" "When a new post fits one of yours."
                    account'.switches.matches \on -> _ { matches = on }
                , switch' "messages" "Messages" "When someone writes, once per conversation until you read it."
                    account'.switches.messages \on -> _ { messages = on }
                , switch' "renewals" "Renewals" "Before one of your posts expires, with one click to renew it."
                    account'.switches.renewals \on -> _ { renewals = on }
                ]
            ]
            <> maybe [] (hint >>> pure) held

        -- Blocked players are listed here rather than behind a button of their
        -- own: the list is a few names at most, and a post a block hides links
        -- here (brief 10, 11.1).
        blockedValue blocked =
            if null blocked
            then [ muted "Nobody is blocked." ]
            else
                [ personRows $ blocked <#> \who ->
                    personRow who $ button Text Small (unblock' who) [ HH.text "Unblock" ]
                , hint "Neither of you sees the other's posts or messages. Unblocking brings all of it back."
                ]

        deleteHtml account' =
            if state.confirmingDelete
            then confirm
                { id: "delete"
                , title: "Delete your account?"
                , text: deleteText account'
                , action: [ Icons.trash2, HH.text "Delete account" ]
                , onConfirm: deleteAccount
                , cancel: "Keep it"
                , onCancel: keepAccount
                }
            else HH.div [ HS.class_ "account-actions account-actions-end" ]
                [ button Destructive Small askDelete [ Icons.trash2, HH.text "Delete account" ] ]

        privateSection { account: account', blocked } =
            HH.section [ HS.class_ "account-section", HPA.labelledBy "private-title" ]
            [ HH.h2 [ HP.id "private-title" ] [ HH.text "Only you see this" ]
            , HH.p [ HS.class_ "account-lead" ] [ HH.text "None of it shows on your posts." ]
            , dataList
                [ { id: Just "email", label: "Email", value: emailValue account', action: Nothing }
                , { id: Nothing, label: "Sign-in", value: signInValue account', action: Nothing }
                , { id: Just "emails", label: "Emails", value: emailsValue account', action: Nothing }
                , { id: Just "blocked", label: "Blocked", value: blockedValue blocked, action: Nothing }
                ]
            , deleteHtml account'
            ]

    Hooks.pure $ HH.div [ HS.class_ "account" ] case state.page of
        Loaded loaded ->
            [ HH.h1_ [ HH.text "Account" ]
            , factsSection loaded
            , privateSection loaded
            , toasts toast dismissToast
            ]
        Failed ->
            [ HH.h1_ [ HH.text "Account" ]
            , HH.p_ [ HH.text "There has been an error loading your account." ]
            ]
        _ -> []

account :: ∀ action slots left. Int -> H.ComponentHTML action (account :: Slot__I Int | slots) (Async left)
account visit = HH.slot_ (Proxy :: _ "account") visit component unit

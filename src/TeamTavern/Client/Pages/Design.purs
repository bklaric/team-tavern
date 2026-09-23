module TeamTavern.Client.Pages.Design (design) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array (delete, elem, range, snoc)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (onMatch)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.AccountFact (accountFact)
import TeamTavern.Client.Components.Button (Size(..), Weight(..), button, iconButton)
import TeamTavern.Client.Components.Card (Place(..), Viewer, card, ownCard)
import TeamTavern.Client.Components.Check (check, choiceList, choices, switch, switches)
import TeamTavern.Client.Components.Confirm (confirm)
import TeamTavern.Client.Components.ContactPanel (Revealed(..), contactPanelSheet)
import TeamTavern.Client.Components.DataList (dataList, personRow, personRows, row)
import TeamTavern.Client.Components.Divider (divider, rule, tierHeading)
import TeamTavern.Client.Components.Field (Labelling(..), field, field_, formSection)
import TeamTavern.Client.Components.Input (Option, input, select, textarea)
import TeamTavern.Client.Components.Menu (menuDivider, menuItem, menuItemDestructive, menuLabel, sheetMenu)
import TeamTavern.Client.Components.Overlay (Presentation(..), overlay, sidePanel, useOverlay)
import TeamTavern.Client.Components.OwnPostStatus (ownPostStatus, renewDue)
import TeamTavern.Client.Components.Pills (pills)
import TeamTavern.Client.Components.Range (ageRange, hoursHint, hoursRange, optionRange)
import TeamTavern.Client.Components.Stepper (countRow, stepper)
import TeamTavern.Client.Components.Toast (toasts, useToast)
import TeamTavern.Client.Components.Tokens as Tokens
import TeamTavern.Client.Components.Unread (badge, unreadDot)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Pages.Design.Cards (fixtures)
import TeamTavern.Client.Script.Expand (toggleCard)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Shared.Fetch (fetchPath)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Game.ViewGame (ViewGame)
import TeamTavern.Routes.Game.ViewGame as ViewGame
import TeamTavern.Shared.Languages (allLanguages)
import Type.Proxy (Proxy(..))

-- Every component of the design system in every state, as the prototype's
-- components.html shows them, to be checked by eye and screenshotted. Each
-- control works, so the keyboard can be run through it. Not linked from
-- anywhere.

data Which = ModalOverlay | SideOverlay | FullScreenOverlay | BottomOverlay | DropdownOverlay

derive instance Eq Which

type State =
    { overlay :: Maybe Which
    , roles :: Array String
    , server :: String
    , languages :: Array String
    , country :: String
    , changingCountry :: Boolean
    , rankFrom :: String
    , rankTo :: String
    , ageFrom :: String
    , ageTo :: String
    , hoursFrom :: String
    , hoursTo :: String
    , size :: Int
    , total :: Int
    , discord :: String
    , communityName :: String
    , words :: String
    , microphone :: Boolean
    , reach :: String
    , postType :: String
    , matches :: Boolean
    , messages :: Boolean
    , renewals :: Boolean
    , confirming :: Boolean
    , viewer :: Maybe Viewer
    , valorant :: Maybe ViewGame.OkContent
    , valheim :: Maybe ViewGame.OkContent
    , expanded :: Array String
    }

initialState :: State
initialState =
    { overlay: Nothing
    , roles: [ "lurker", "supporter" ]
    , server: "dedicated"
    , languages: [ "Croatian", "English" ]
    , country: "Croatia"
    , changingCountry: false
    , rankFrom: "Gold 1"
    , rankTo: ""
    , ageFrom: "18"
    , ageTo: ""
    , hoursFrom: "21:00"
    , hoursTo: "01:00"
    , size: 4
    , total: 4
    , discord: "kestrel.gg"
    , communityName: ""
    , words: ""
    , microphone: true
    , reach: "message"
    , postType: "group"
    , matches: true
    , messages: true
    , renewals: false
    , confirming: false
    , viewer: Nothing
    , valorant: Nothing
    , valheim: Nothing
    , expanded: [ "state-expanded", "state-community-expanded" ]
    }

roleOptions :: Array Option
roleOptions =
    [ { value: "leader", label: "In-game leader" }
    , { value: "lurker", label: "Lurker" }
    , { value: "supporter", label: "Supporter" }
    ]

serverOptions :: Array Option
serverOptions =
    [ { value: "dedicated", label: "Dedicated server" }
    , { value: "discord", label: "Discord server" }
    , { value: "clan", label: "Clan" }
    ]

languageOptions :: Array Option
languageOptions = allLanguages <#> \language -> { value: language, label: language }

countryOptions :: Array Option
countryOptions =
    [ "Croatia", "Germany", "Poland", "Serbia", "Slovenia", "United Kingdom" ]
    <#> \country -> { value: country, label: country }

rankOptions :: Array Option
rankOptions =
    ( [ "Iron", "Bronze", "Silver", "Gold", "Platinum", "Diamond", "Ascendant", "Immortal" ]
        >>= \tier -> range 1 3 <#> \division -> tier <> " " <> show division
    )
    <> [ "Radiant" ]
    <#> \rank -> { value: rank, label: rank }

reachOptions :: Array Option
reachOptions =
    [ { value: "message", label: "Message me on TeamTavern" }
    , { value: "discord", label: "Add me on Discord or in game" }
    ]

tokens :: Array { name :: String, variable :: String }
tokens =
    [ "floor", "table", "raised", "border", "input-border", "text", "text-muted", "text-faint"
    , "ember", "ember-hover", "on-ember", "moss", "error"
    ]
    <#> \name -> { name, variable: "--" <> name }

caption :: ∀ w i. String -> HH.HTML w i
caption text = HH.p [ HS.class_ "sheet-caption" ] [ HH.text text ]

section :: ∀ w i. String -> String -> Array (HH.HTML w i) -> HH.HTML w i
section heading note content =
    HH.section_ $ [ HH.h2_ [ HH.text heading ], HH.p [ HS.class_ "sheet-note" ] [ HH.text note ] ] <> content

sheetRow :: ∀ w i. Array (HH.HTML w i) -> HH.HTML w i
sheetRow = HH.div [ HS.class_ "sheet-row" ]

toggle :: String -> Array String -> Array String
toggle value values = if elem value values then delete value values else snoc values value

component :: ∀ query input output left. H.Component query input output (Async left)
component = Hooks.component \_ _ -> Hooks.do
    state /\ stateId <- Hooks.useState initialState
    { toast, showToast, dismissToast } <- useToast

    -- The cards read the seeded games' fields, as the feed's do.
    Hooks.useLifecycleEffect do
        now' <- liftEffect now
        timezone <- getClientTimezone
        Hooks.modify_ stateId _ { viewer = Just { now: now', timezone } }
        let load handle put = void $ Hooks.fork do
                result <- H.lift $ Async.attempt $ fetchPath (Proxy :: _ ViewGame) { handle }
                case result of
                    Right response -> response # onMatch { ok: put } (const $ pure unit)
                    Left _ -> pure unit
        load "valorant" \game -> Hooks.modify_ stateId _ { valorant = Just game }
        load "valheim" \game -> Hooks.modify_ stateId _ { valheim = Just game }
        pure Nothing

    let set = Hooks.modify_ stateId
        open which = set _ { overlay = Just which }
        close = set _ { overlay = Nothing }
        isOpen which = state.overlay == Just which
        menuPresentation = Dropdown { className: "menu", role: "menu" }
        refs =
            { modal: H.RefLabel "design-modal"
            , side: H.RefLabel "design-side"
            , fullScreen: H.RefLabel "design-full-screen"
            , bottom: H.RefLabel "design-bottom"
            , dropdown: H.RefLabel "design-dropdown"
            }
        report = close *> showToast { text: "Danya is reported.", action: Nothing }
        block = close *> showToast
            { text: "Danya is blocked."
            , action: Just { label: "Undo", onAction: showToast { text: "Danya is unblocked.", action: Nothing } }
            }
        menuItems =
            [ menuItem report [ Icons.flag, HH.text "Report" ]
            , menuItemDestructive block [ Icons.ban, HH.text "Block Danya" ]
            ]

    useOverlay refs.modal Modal (isOpen ModalOverlay) close
    useOverlay refs.side Side (isOpen SideOverlay) close
    useOverlay refs.fullScreen FullScreen (isOpen FullScreenOverlay) close
    useOverlay refs.bottom Bottom (isOpen BottomOverlay) close
    useOverlay refs.dropdown menuPresentation (isOpen DropdownOverlay) close

    let colors = section "Color"
            "Stone for surfaces, wood for cards, moss for what fits, ember for the one thing that asks for a click."
            [ HH.div [ HS.class_ "swatches" ] $ tokens <#> \{ name, variable } ->
                HH.div [ HS.class_ "swatch" ]
                [ HH.div [ HS.class_ "swatch-color", HP.style $ "background: var(" <> variable <> ")" ] []
                , HH.div [ HS.class_ "swatch-label" ] [ HH.text name, HH.span_ [ HH.text variable ] ]
                ]
            ]

        specimen text styleNote style =
            HH.div_
            [ HH.div [ HP.style style ] [ HH.text text ]
            , HH.span [ HS.class_ "muted" ] [ HH.text styleNote ]
            ]

        digits class_ label =
            HH.div_
            [ HH.div [ HS.class_ class_ ] [ HH.span_ [ HH.text "19–23" ], HH.span_ [ HH.text "08–12" ], HH.span_ [ HH.text "21–01" ] ]
            , HH.div [ HS.class_ "muted" ] [ HH.text label ]
            ]

        type_ = section "Type" "Inter, five sizes, three weights."
            [ HH.div [ HS.class_ "type-specimen" ]
                [ HH.div_ [ HH.h1_ [ HH.text "Find players, groups and communities" ], HH.span [ HS.class_ "muted" ] [ HH.text "28 / 600: page titles" ] ]
                , HH.div_ [ HH.h2_ [ HH.text "Your post is live" ], HH.span [ HS.class_ "muted" ] [ HH.text "20 / 600: section headings" ] ]
                , HH.div_ [ HH.h3_ [ HH.text "Night Owls" ], HH.span [ HS.class_ "muted" ] [ HH.text "16 / 600: card names" ] ]
                , specimen "Diamond 2 · Duelist, Initiator · Croatia · EN, DE" "14 / 500: the fact line" "font-weight: 500"
                , specimen "Three friends who play most nights, we want to stop solo queuing." "14 / 400: body and the player's words" ""
                , specimen "Active 2 days ago" "12 / 500: labels" "font-size: 12px; font-weight: 500"
                , specimen "Ищу команду · Ψάχνω ομάδα · Szukam ekipy · Tìm đồng đội · Suche Gruppe" "Scripts players write in" ""
                , HH.div [ HS.class_ "sheet-row sheet-digits" ]
                    [ digits "digits" "Proportional", digits "digits tabular" "Tabular, for hours, ages and counts" ]
                ]
            ]

        buttons = section "Buttons" "Filled ember once per screen. Card contact buttons are outlined."
            [ sheetRow
                [ button Primary Regular (pure unit) [ HH.text "Publish post" ]
                , button Outline Small (pure unit) [ Icons.plus, HH.text "New post" ]
                , button Outline Small (pure unit) [ Icons.discord, HH.text "Add on Discord" ]
                , button Outline Small (pure unit) [ Icons.messageCircle, HH.text "Message" ]
                , button Outline Small (pure unit) [ Icons.externalLink, HH.text "Visit site" ]
                , button Text Small (pure unit) [ HH.text "Details ", Icons.chevronDown ]
                , button Destructive Regular (pure unit) [ HH.text "Delete account" ]
                ]
            , caption "Icon buttons carry their label for screen readers"
            , sheetRow
                [ iconButton "Close" (pure unit) Icons.x
                , iconButton "Menu" (pure unit) Icons.menu
                , iconButton "More" (pure unit) Icons.ellipsis
                ]
            ]

        opener which label =
            button Outline Regular (open which) [ HH.text label ]

        overlays = section "Overlays"
            "One overlay, presented as the screen calls for: a modal or a side panel on a desktop, the whole screen on a phone, a sheet from the bottom for a menu of a few short rows, and a dropdown from the button that opened it. It takes the focus when it opens, keeps Tab inside it while the page behind is inert, closes on Escape, and gives the focus back when it closes."
            [ sheetRow
                [ opener ModalOverlay "Modal"
                , opener SideOverlay "Side panel"
                , opener FullScreenOverlay "Full screen"
                , opener BottomOverlay "Sheet from the bottom"
                , HH.div [ HS.class_ "menu-wrap" ]
                    [ HH.button
                        [ HS.class_ "button button-outline"
                        , HP.type_ HP.ButtonButton
                        , HPA.hasPopup "menu"
                        , HPA.expanded $ show $ isOpen DropdownOverlay
                        , HE.onClick $ const if isOpen DropdownOverlay then close else open DropdownOverlay
                        ]
                        [ HH.text "Dropdown ", Icons.chevronDown ]
                    , if isOpen DropdownOverlay
                        then overlay { ref: refs.dropdown, presentation: menuPresentation, title: "Danya", onClose: close }
                            ([ menuLabel "Danya" ] <> menuItems) []
                        else HH.text ""
                    ]
                ]
            ]

        overlayLayers =
            [ if isOpen ModalOverlay
                then overlay { ref: refs.modal, presentation: Modal, title: "Preview", onClose: close }
                    [ HH.p_ [ HH.text "A modal from 640 px, centred over the page; the whole screen on a phone." ]
                    , field (field_ "design-modal-name" "Community name")
                        [ input [ HP.id "design-modal-name" ]
                            { value: state.communityName, placeholder: "", onInput: \value -> set _ { communityName = value } }
                        ]
                    ]
                    [ button Text Regular close [ HH.text "Keep editing" ]
                    , button Primary Regular close [ HH.text "Publish post" ]
                    ]
                else HH.text ""
            , if isOpen SideOverlay
                then sidePanel
                    { ref: refs.side, title: "Night Owls", subtitle: "Valorant group · Posted by Kestrel", tools: [], onClose: close }
                    [ HH.p_ [ HH.text "A side panel from 640 px, standing at the page's right edge; the whole screen on a phone." ]
                    , HH.div [ HS.class_ "sheet-row" ]
                        [ button Outline Small (pure unit) [ Icons.copy, HH.text "Copy" ]
                        , button Outline Small (pure unit) [ Icons.discord, HH.text "Open the invite" ]
                        ]
                    ]
                else HH.text ""
            , if isOpen FullScreenOverlay
                then overlay { ref: refs.fullScreen, presentation: FullScreen, title: "Tell us about you", onClose: close }
                    [ HH.p_ [ HH.text "The whole screen, which every presentation but the dropdown is on a phone." ]
                    , field ((field_ "design-sheet-roles" "Roles") { labelling = Group })
                        [ pills
                            { id: "design-sheet-roles", multiple: true, options: roleOptions, chosen: state.roles
                            , onToggle: \value -> set \state' -> state' { roles = toggle value state'.roles }
                            , all: Nothing
                            }
                        ]
                    ]
                    [ button Primary Regular close [ HH.text "Show posts" ] ]
                else HH.text ""
            , if isOpen BottomOverlay
                then overlay { ref: refs.bottom, presentation: Bottom, title: "Danya", onClose: close }
                    [ sheetMenu $ menuItems <> [ menuDivider, menuItem close [ HH.text "Cancel" ] ] ]
                    []
                else HH.text ""
            ]

        rolesField =
            field ((field_ "design-roles" "Roles") { labelling = Group })
            [ pills
                { id: "design-roles", multiple: true, options: roleOptions, chosen: state.roles
                , onToggle: \value -> set \state' -> state' { roles = toggle value state'.roles }
                , all: Just { label: "Any role", onAll: set _ { roles = roleOptions <#> _.value } }
                }
            ]

        serverField =
            field ((field_ "design-server" "Server") { labelling = Group })
            [ pills
                { id: "design-server", multiple: false, options: serverOptions, chosen: [ state.server ]
                , onToggle: \value -> set _ { server = value }
                , all: Nothing
                }
            ]

        languagesField =
            field ((field_ "design-languages" "Languages") { labelling = Group })
            [ Tokens.tokens
                { id: "design-languages", one: "language", options: languageOptions, chosen: state.languages }
                \languages -> set _ { languages = languages }
            ]

        countryField =
            field ((field_ "design-country" "Location") { labelling = if state.changingCountry then For else Group })
            [ if state.changingCountry
                then select [ HP.id "design-country" ]
                    { options: countryOptions, value: state.country, placeholder: Just "Choose a country"
                    , onChange: \value -> set _ { country = value }
                    }
                else accountFact state.country $ set _ { changingCountry = true }
            ]

        rankField =
            field ((field_ "design-rank" "Rank range") { labelling = Group })
            [ optionRange rankOptions
                { from: state.rankFrom, to: state.rankTo
                , onFrom: \value -> set _ { rankFrom = value }, onTo: \value -> set _ { rankTo = value }
                }
            ]

        ageField =
            field ((field_ "design-age" "Ages") { labelling = Group })
            [ ageRange
                { from: state.ageFrom, to: state.ageTo
                , onFrom: \value -> set _ { ageFrom = value }, onTo: \value -> set _ { ageTo = value }
                }
            ]

        hoursField =
            field ((field_ "design-hours" "Online") { labelling = Group, hint = Just hoursHint })
            [ hoursRange
                { from: state.hoursFrom, to: state.hoursTo
                , onFrom: \value -> set _ { hoursFrom = value }, onTo: \value -> set _ { hoursTo = value }
                }
            ]

        sizeRow =
            countRow
            [ stepper
                { label: "Players in the group", value: state.size, min: 1, max: state.total
                , onStep: \value -> set _ { size = value }
                }
            , HH.span [ HS.class_ "muted" ] [ HH.text "of" ]
            , stepper
                { label: "Players in all", value: state.total, min: max 2 state.size, max: 10
                , onStep: \value -> set _ { total = value }
                }
            ]

        typeChoices =
            choices
            { name: "design-type"
            , label: "You are"
            , options:
                [ { value: "player", label: "I'm a player looking for a group", icon: Icons.user }
                , { value: "group", label: "We're a group looking for players", icon: Icons.users }
                , { value: "community", label: "We're a community looking for members", icon: Icons.castle }
                ]
            , chosen: state.postType
            , onChoose: \value -> set _ { postType = value }
            }

        inputs = section "Inputs"
            "The controls of the post screen and the description bar. A chosen pill takes the text color, not ember: it is an answer, not a call to act."
            [ caption "Radio cards: the three post types"
            , typeChoices
            , caption "Pills, several or one"
            , formSection Nothing [ rolesField, serverField ]
            , caption "Tokens, for a few out of many"
            , formSection Nothing [ languagesField ]
            , caption "Ranges: over a field's ordered options, of ages, and of hours that can cross midnight"
            , formSection Nothing [ rankField, ageField, hoursField ]
            , caption "Count stepper: a group of 4 of 4 can't grow its members until it wants more"
            , sizeRow
            , caption "Fields: an account fact, a changed one, an error"
            , formSection Nothing
                [ countryField
                , field ((field_ "design-discord" "Discord") { note = Just "Applies to all your posts" })
                    [ input [ HP.id "design-discord" ]
                        { value: state.discord, placeholder: "Your Discord username", onInput: \value -> set _ { discord = value } }
                    ]
                , field
                    ( (field_ "design-name" "Community name")
                        { error = if state.communityName == "" then Just "Give your community a name." else Nothing }
                    )
                    [ input [ HP.id "design-name" ]
                        { value: state.communityName, placeholder: "", onInput: \value -> set _ { communityName = value } }
                    ]
                , field
                    ( (field_ "design-words" "Tell people about your community")
                        { required = true
                        , hint = Just "Ideas: What do members do together? How big are you, and what are the rules?"
                        }
                    )
                    [ textarea [ HP.id "design-words" ]
                        { value: state.words
                        , placeholder: "Weekly events, a friendly Discord and admins online most evenings."
                        , onInput: \value -> set _ { words = value }
                        }
                    ]
                , check
                    { id: "design-microphone", text: "Microphone required", checked: state.microphone
                    , onChange: \checked -> set _ { microphone = checked }
                    }
                , field ((field_ "design-reach" "How should people reach you?") { labelling = Group })
                    [ choiceList
                        { id: "design-reach", options: reachOptions, chosen: state.reach
                        , onChoose: \value -> set _ { reach = value }
                        }
                    ]
                ]
            , caption "Switches: on or off the moment they are flipped"
            , switches
                [ switch
                    { id: "design-matches", text: "Matches", note: Just "When a new post fits one of yours."
                    , checked: state.matches, onChange: \checked -> set _ { matches = checked }
                    }
                , switch
                    { id: "design-messages", text: "Messages"
                    , note: Just "When someone writes, once per conversation until you read it."
                    , checked: state.messages, onChange: \checked -> set _ { messages = checked }
                    }
                , switch
                    { id: "design-renewals", text: "Renewals"
                    , note: Just "Before one of your posts expires, with one click to renew it."
                    , checked: state.renewals, onChange: \checked -> set _ { renewals = checked }
                    }
                ]
            ]

        deletePost = set _ { confirming = false } *> showToast
            { text: "Night Owls is deleted."
            , action: Just { label: "Undo", onAction: showToast { text: "Night Owls is back.", action: Nothing } }
            }

        confirmations = section "Confirmations and toasts"
            "A confirmation stands in place of the button that asked for it and says what will be lost. A toast follows an action, with Undo where it can be undone."
            [ caption "A confirmation with the counts"
            , if state.confirming
                then confirm
                    { id: "design-confirm"
                    , title: "Delete Night Owls?"
                    , text: "3 conversations will be deleted for both of you."
                    , action: [ Icons.trash2, HH.text "Delete post" ]
                    , onConfirm: deletePost
                    , cancel: "Keep it"
                    , onCancel: set _ { confirming = false }
                    }
                else sheetRow [ button Destructive Regular (set _ { confirming = true }) [ Icons.trash2, HH.text "Delete post" ] ]
            , caption "A toast, with Undo where the action can be undone"
            , sheetRow
                [ HH.div [ HS.class_ "toast" ]
                    [ HH.span [ HS.class_ "toast-text" ] [ HH.text "Danya is blocked." ]
                    , button Text Small (pure unit) [ HH.text "Undo" ]
                    ]
                , button Outline Small block [ Icons.ban, HH.text "Block Danya" ]
                , button Outline Small (showToast { text: "Copied.", action: Nothing }) [ Icons.copy, HH.text "Copy" ]
                ]
            ]

        feed = section "Feed structure"
            "Tiers with their counts, the divider above the posts that have expired, and a labelled rule between two ways to do one thing."
            [ tierHeading "Fits you" (Just 3)
            , tierHeading "Missing one thing" (Just 12)
            , tierHeading "Missing more" Nothing
            , divider "Older posts · they may no longer be looking"
            , rule "or"
            ]

        contactPanels = section "Contact panel"
            "Every contact button opens one panel, with what the owner shared and the conversation about the post. What the owner prefers comes first; a conversation under way always does."
            case state.viewer of
            Just viewer -> let
                posts = fixtures viewer.now
                sheet key label post contacts =
                    [ caption label
                    , contactPanelSheet (H.RefLabel $ "design-panel-" <> key)
                        { now: viewer.now
                        , panel:
                            { game: { handle: "valorant", title: "Valorant" }
                            , post
                            , revealed: Revealed contacts
                            , copied: if key == "offsite" then Just post.owner else Nothing
                            }
                        , onClose: pure unit
                        , onCopy: const $ pure unit
                        }
                    ]
                none = { contacts: [], discord_server: Nothing, website: Nothing }
                in
                sheet "message" "The owner prefers messages: the message box first, their contacts after" posts.nightOwls
                    none { contacts = [ { kind: "discord", value: "kestrel" }, { kind: "riot", value: "Kestrel#EUW" } ] }
                <> sheet "offsite" "The owner prefers Discord: contacts first, and Send is outlined, with one just copied" posts.shadowFox
                    none { contacts = [ { kind: "discord", value: "ShadowFox" } ] }
                <> sheet "community" "A community joined on Discord: the invite is the one filled button"
                    posts.radiantRising { contacts = [] }
                    none { discord_server = Just "discord.gg/radiantrising", website = Just "radiantrising.gg" }
                <> sheet "older" "An older post with nothing to reveal: its owner may no longer be looking"
                    posts.expiredPlayer { contacts = [] }
                    none
            Nothing -> []

        unread = section "Unread"
            "A count on an icon, read out from the button's label, and a dot on a row."
            [ sheetRow
                [ HH.button
                    [ HS.class_ "icon-button header-count", HP.type_ HP.ButtonButton, HPA.label "Messages, 2 unread" ]
                    [ Icons.mail, badge 2 ]
                , HH.button
                    [ HS.class_ "icon-button header-count", HP.type_ HP.ButtonButton, HPA.label "Notifications, 12 unread" ]
                    [ Icons.bell, badge 12 ]
                , HH.span [ HS.class_ "sheet-unread" ] $ unreadDot <> [ HH.text "Vex fits · just now" ]
                ]
            ]

        cards = section "Cards"
            "One shell for the three post types. Marked, the facts carry what fits the viewer's description: a Valorant Controller at Diamond 1 in Croatia, speaking English, online 19:00–23:00, looking for ranked games."
            case state.viewer, state.valorant, state.valheim of
            Just viewer, Just valorant, Just valheim -> let
                posts = fixtures viewer.now
                cardOf key game { marked, place } post = card
                    { game
                    , viewer
                    , post
                    , marked
                    , expanded: elem key state.expanded
                    , place
                    , onToggle: \event -> toggleCard event $ set \state' -> state' { expanded = toggle key state'.expanded }
                    , onContact: pure unit
                    , onEdit: pure unit
                    , onRenew: pure unit
                    }
                feedCard key = cardOf key valorant { marked: true, place: Listed }
                cardState label key game post = [ caption label, cardOf key game { marked: false, place: Listed } post ]
                in
                [ caption "A feed, marked"
                , HH.div [ HS.class_ "feed-stack" ]
                    [ tierHeading "Fits you" (Just 3)
                    , feedCard "feed-night-owls" posts.nightOwls
                    , feedCard "feed-radiant-rising" posts.radiantRising
                    , feedCard "feed-shadow-fox" posts.shadowFox
                    , tierHeading "Missing one thing" (Just 12)
                    , feedCard "feed-afterglow" posts.afterglow
                    , tierHeading "Missing more" Nothing
                    , feedCard "feed-lumen" posts.lumen
                    , divider "Older posts · they may no longer be looking"
                    , feedCard "feed-expired-player" posts.expiredPlayer
                    , feedCard "feed-expired-group" posts.expiredGroup
                    ]
                ]
                <> cardState "Expanded" "state-expanded" valorant posts.shadowFox
                <> cardState "A community, expanded" "state-community-expanded" valheim posts.farlands
                <> cardState "A community, collapsed" "state-community" valheim posts.farlands
                <> cardState "A group with no name, on a server game" "state-unnamed" valheim posts.valheimGroup
                <> cardState "Your own post" "state-own" valorant posts.ownNightOwls
                <> cardState "Already messaged" "state-messaged" valorant posts.messagedShadowFox
                <> [ caption "A preview on the post screen"
                    , cardOf "state-preview" valorant { marked: false, place: Preview } posts.nightOwls
                    , caption "On its own page"
                    , cardOf "state-page" valorant { marked: false, place: Page { blocked: false, status: [] } } posts.shadowFox
                    , caption "Your own post on its own page"
                    , cardOf "state-page-own" valorant
                        { marked: false
                        , place: Page
                            { blocked: false
                            , status:
                                [ ownPostStatus
                                    { now: viewer.now, expires: posts.ownExpires, conversations: 3, unread: 1, reveals: 14 }
                                ]
                            }
                        }
                        posts.ownNightOwls
                    , caption "Your own post on the home page"
                    , ownCard
                        { game: valorant
                        , viewer
                        , post: posts.ownNightOwls
                        , status: ownPostStatus
                            { now: viewer.now, expires: posts.ownExpires, conversations: 3, unread: 1, reveals: 14 }
                        , renewDue: renewDue viewer.now posts.ownExpires
                        , onFits: const $ pure unit
                        , onRenew: pure unit
                        }
                    , caption "On its own page, blocked"
                    , cardOf "state-page-blocked" valorant { marked: false, place: Page { blocked: true, status: [] } } posts.nightOwls
                    ]
                <> cardState "A long name, every role and Cyrillic words" "state-stress" valorant posts.stress
                <> cardState "Almost empty" "state-sparse" valorant posts.sparse
            _, _, _ -> [ caption "Loading the games' fields." ]

        textButton label = button Text Small (pure unit) [ HH.text label ]

        account = section "Definition list"
            "One definition list per section of the account page: a label, what the account holds, and where there is one, the way to change it."
            [ dataList
                [ row "Nickname" [ HH.text "Kestrel" ]
                , row "Birthday" [ HH.text "12 April 1998, shown as age 28" ]
                , row "Languages" [ HH.text "Croatian, English" ]
                , (row "Email" [ HH.text "kestrel@example.com" ]) { action = Just $ textButton "Change" }
                , (row "Email"
                    [ HH.text "mira@example.com"
                    , HH.p [ HS.class_ "field-hint" ]
                        [ HH.text "Not confirmed yet. We sent it a link, and send it nothing else until the link is clicked." ]
                    , textButton "Send again"
                    ]) { action = Just $ textButton "Change" }
                , (row "Sign-in" [ HH.span [ HS.class_ "data-line" ] [ Icons.discord, HH.text "Discord" ] ])
                    { action = Just $ textButton "Use a password" }
                , (row "Blocked"
                    [ personRows [ personRow "Danya" $ textButton "Unblock", personRow "Quill" $ textButton "Unblock" ]
                    , HH.p [ HS.class_ "field-hint" ]
                        [ HH.text "Neither of you sees the other's posts or messages. Unblocking brings all of it back." ]
                    ]) { id = Just "design-blocked" }
                ]
            ]

    Hooks.pure $
        HH.div_ $
        [ HH.main [ HS.class_ "sheet" ]
            [ HH.h1_ [ HH.text "TeamTavern design system" ]
            , HH.p [ HS.class_ "sheet-note sheet-lead" ] [ HH.text "Every component in every state. Brief section 14." ]
            , colors
            , type_
            , buttons
            , overlays
            , inputs
            , confirmations
            , feed
            , cards
            , contactPanels
            , unread
            , account
            ]
        ]
        <> overlayLayers
        <> [ toasts toast dismissToast ]

design :: ∀ action slots left. H.ComponentHTML action (design :: Slot___ | slots) (Async left)
design = HH.slot_ (Proxy :: _ "design") unit component unit

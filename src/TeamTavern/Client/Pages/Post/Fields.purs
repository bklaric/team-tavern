module TeamTavern.Client.Pages.Post.Fields
    ( Context
    , Html
    , cardFields
    , contactFields
    , contactKeys
    , wordsField
    ) where

import Prelude

import Data.Array (delete, elem, filter, index, null, snoc, sortBy)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll, split, toLower)
import Data.String.CodeUnits as CodeUnits
import Effect.Class (class MonadEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM)
import TeamTavern.Client.Components.AccountFact (accountFact)
import TeamTavern.Client.Components.Button (Size(..), Weight(..), button)
import TeamTavern.Client.Components.Card (flagText)
import TeamTavern.Client.Components.Check (check, choiceList)
import TeamTavern.Client.Components.Field (Labelling(..), field, field_)
import TeamTavern.Client.Components.Field as Form
import TeamTavern.Client.Components.Input (Option, input, select, textarea)
import TeamTavern.Client.Components.Pills (pills)
import TeamTavern.Client.Components.Range (ageRange, hoursHint, hoursRange, optionRange)
import TeamTavern.Client.Components.Stepper (countRow, stepper)
import TeamTavern.Client.Components.Tokens as Tokens
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Pages.Post.Draft (Draft)
import TeamTavern.Client.Shared.Contacts (contactLabel, contactPlaceholder)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Game.ViewGame as ViewGame
import TeamTavern.Routes.Shared.Field (Field)
import TeamTavern.Routes.Shared.Post (AccountContent)
import TeamTavern.Shared.Languages (allLanguages)
import TeamTavern.Shared.Timezones (allTimezones)

type Html m slots = H.ComponentHTML (HookM m Unit) (tokens :: Tokens.Slot | slots) m

-- | What the fields are drawn from. `account` is the signed-in player's,
-- | whose facts show as their values until Change (`changing`) opens them;
-- | `otherPosts` is whether changing one changes other posts too. `onChange`
-- | names the field it changes, so the field's error goes with the change.
-- | `onDiscord` signs up with Discord, which signed out the Discord input
-- | offers (brief 6, step 3).
type Context m =
    { game :: ViewGame.OkContent
    , type_ :: String
    , draft :: Draft
    , account :: Maybe AccountContent
    , nickname :: Maybe String
    , otherPosts :: Boolean
    , changing :: Array String
    , errors :: Object String
    , countries :: Array String
    , regions :: Array String
    , timezone :: String
    , onChange :: String -> (Draft -> Draft) -> HookM m Unit
    , onUnfold :: String -> HookM m Unit
    , onDiscord :: HookM m Unit
    }

idOf :: String -> String
idOf key = "post-" <> key

errorOf :: ∀ m. Context m -> String -> Maybe String
errorOf context key = Object.lookup key context.errors

-- A field with nothing below it but the error the screen has for it.
shell :: ∀ m. Context m -> String -> String -> Labelling -> Maybe String -> Form.Field
shell context key label labelling hint =
    (field_ (idOf key) label) { labelling = labelling, hint = hint, error = errorOf context key }

toggled :: String -> Array String -> Array String
toggled value values = if elem value values then delete value values else snoc values value

article :: String -> String
article word = if elem (CodeUnits.take 1 $ toLower word) [ "a", "e", "i", "o", "u" ] then "an" else "a"

plural :: String -> String
plural word = if CodeUnits.takeRight 1 word == "s" then word <> "es" else word <> "s"

optionsOf :: Field -> Array Option
optionsOf gameField = gameField.options <#> \{ key, label } -> { value: key, label }

-- Account facts: what the account holds, and what the draft says, for each
-- fact the screen can fold.

data Fact = Location | Languages | Birthday | Timezone | Contact String

factKey :: Fact -> String
factKey Location = "location"
factKey Languages = "languages"
factKey Birthday = "birthday"
factKey Timezone = "timezone"
factKey (Contact kind) = kind

accountValue :: AccountContent -> Fact -> Maybe String
accountValue account = case _ of
    Location -> account.country
    Languages -> if null account.languages then Nothing else Just $ joinWith ", " account.languages
    Birthday -> account.birthday
    Timezone -> account.timezone
    Contact kind -> Object.lookup kind account.contacts

draftValue :: Draft -> Fact -> Maybe String
draftValue draft = case _ of
    Location -> draft.country
    Languages -> if null draft.languages then Nothing else Just $ joinWith ", " draft.languages
    Birthday -> draft.birthday
    Timezone -> draft.timezone
    Contact kind -> Object.lookup kind draft.contacts >>= \value -> if value == "" then Nothing else Just value

months :: Array String
months =
    [ "January", "February", "March", "April", "May", "June"
    , "July", "August", "September", "October", "November", "December"
    ]

-- How a fact reads where it is shown rather than asked for.
factText :: Fact -> String -> String
factText Birthday value = case split (Pattern "-") value <#> Int.fromString of
    [ Just year, Just month, Just day ] ->
        show day <> " " <> fromMaybe "" (index months (month - 1)) <> " " <> show year
    _ -> value
factText Timezone value = replaceAll (Pattern "_") (Replacement " ") value
factText _ value = value

-- A fact the account holds shows as its value, "Croatia · From your
-- account · Change", until the player changes it here. Changing one changes it
-- on every post the player owns, which a player with other posts is told.
accountField :: ∀ m slots. MonadEffect m => Context m -> Fact -> String -> Labelling -> Maybe String -> Html m slots -> Html m slots
accountField context fact label labelling hint control = let
    key = factKey fact
    held = context.account >>= flip accountValue fact
    drafted = draftValue context.draft fact
    changing = elem key context.changing
    folded = isJust context.account && not changing && isJust held && held == drafted
    changed = changing || held /= drafted
    in
    if folded
    then field (shell context key label Group Nothing)
        [ accountFact (maybe "" (factText fact) drafted) (context.onUnfold key) ]
    else field
        (shell context key label labelling hint)
            { note = if isJust context.account && context.otherPosts && changed
                then Just "Applies to all your posts" else Nothing
            }
        [ control ]

languagesControl :: ∀ m slots. MonadEffect m => Context m -> Html m slots
languagesControl context =
    Tokens.tokens
        { id: idOf "languages"
        , one: "language"
        , options: allLanguages <#> \language -> { value: language, label: language }
        , chosen: context.draft.languages
        }
        \languages -> context.onChange "languages" _ { languages = languages }

-- The game's fields the post type is asked, in the game's order. A group asks
-- for a range of every ordered field and for the slots it needs; a yes-or-no
-- field is a checkbox, which a group reads as a need.
gameFields :: ∀ m slots. MonadEffect m => Context m -> Array (Html m slots)
gameFields context@{ type_, draft } =
    context.game.fields # filter (_.appliesTo >>> elem type_) <#> \gameField -> let
        key = gameField.key
        chosen = Object.lookup key draft.options # fromMaybe []
        setOptions values = context.onChange key \draft' -> draft'
            { options = if null values
                then Object.delete key draft'.options
                else Object.insert key values draft'.options
            }
        in
        if gameField.ilk == "boolean" then
            field (shell context key gameField.label Unlabelled Nothing)
            [ check
                { id: idOf key
                , text:
                    if type_ == "player"
                    then "I can be " <> article gameField.label <> " " <> toLower gameField.label
                    else flagText type_ gameField true
                , checked: elem key draft.flags
                , onChange: \_ -> context.onChange key \draft' -> draft' { flags = toggled key draft'.flags }
                }
            ]
        else if gameField.ordered && type_ == "player" then
            field (shell context key gameField.label For Nothing)
            [ select [ HP.id $ idOf key ]
                { options: optionsOf gameField
                , value: joinWith "" chosen
                , placeholder: Just $ "Choose your " <> toLower gameField.label
                , onChange: \value -> setOptions if value == "" then [] else [ value ]
                }
            ]
        else if gameField.ordered then let
            range = Object.lookup key draft.ranges # fromMaybe { from: Nothing, to: Nothing }
            setRange change = context.onChange key \draft' -> let
                range' = change $ Object.lookup key draft'.ranges # fromMaybe { from: Nothing, to: Nothing }
                in draft' { ranges = Object.insert key range' draft'.ranges }
            blank value = if value == "" then Nothing else Just value
            in
            field (shell context key (gameField.label <> " range") Group Nothing)
            [ optionRange (optionsOf gameField)
                { from: fromMaybe "" range.from
                , to: fromMaybe "" range.to
                , onFrom: \value -> setRange _ { from = blank value }
                , onTo: \value -> setRange _ { to = blank value }
                }
            ]
        else let
            multiple = gameField.ilk == "multi"
            label = if type_ == "group" && gameField.slotted then plural gameField.label <> " you need" else gameField.label
            in
            field (shell context key label Group Nothing)
            [ pills
                { id: idOf key
                , multiple
                , options: optionsOf gameField
                , chosen
                , onToggle: \value -> setOptions if multiple then toggled value chosen else [ value ]
                , all:
                    if multiple && gameField.slotted
                    then Just { label: "Any " <> toLower gameField.label, onAll: setOptions $ gameField.options <#> _.key }
                    else Nothing
                }
            ]

nameField :: ∀ m slots. MonadEffect m => Context m -> Html m slots
nameField context = let
    community = context.type_ == "community"
    hint
        | community = Nothing
        | otherwise = Just case context.nickname of
            Just nickname -> "Without one, the card says “" <> nickname <> "'s group”."
            Nothing -> "Without one, the card is named after you."
    in
    field ((shell context "name" (if community then "Community name" else "Group name") For hint) { required = community })
    [ input [ HP.id $ idOf "name" ]
        { value: context.draft.name, placeholder: "", onInput: \value -> context.onChange "name" _ { name = value } }
    ]

-- How many the group is and how many more it wants, which the card heading
-- reads back (brief 5.2). The second number is there for a group that will
-- take either, and reads as one where they agree.
sizeField :: ∀ m slots. MonadEffect m => Context m -> Html m slots
sizeField context@{ draft } =
    field (shell context "size" "How many are you, and how many more do you want?" Group Nothing)
    [ countRow
        [ stepper { label: "Players in the group", value: draft.size, min: 1, max: 30
            , onStep: \value -> context.onChange "size" _ { size = value } }
        , HH.span [ HS.class_ "muted" ] [ HH.text "players, want" ]
        , stepper { label: "At least", value: draft.wantedFrom, min: 1, max: 30
            , onStep: \value -> context.onChange "size" _ { wantedFrom = value, wantedTo = max value draft.wantedTo } }
        , HH.span [ HS.class_ "muted" ] [ HH.text "to" ]
        , stepper { label: "At most", value: draft.wantedTo, min: draft.wantedFrom, max: 30
            , onStep: \value -> context.onChange "size" _ { wantedTo = value } }
        , HH.span [ HS.class_ "muted" ] [ HH.text "more" ]
        ]
    ]

regionsField :: ∀ m slots. MonadEffect m => Context m -> Html m slots
regionsField context =
    field (shell context "regions" "Regions" Group (Just "Where the players you're looking for are"))
    [ pills
        { id: idOf "regions"
        , multiple: true
        , options: context.regions <#> \region -> { value: region, label: region }
        , chosen: context.draft.regions
        , onToggle: \value -> context.onChange "regions" \draft -> draft { regions = toggled value draft.regions }
        , all: Nothing
        }
    ]

microphoneField :: ∀ m slots. MonadEffect m => Context m -> Html m slots
microphoneField context =
    field (shell context "mic" "Microphone" Unlabelled Nothing)
    [ check
        { id: idOf "mic"
        , text: if context.type_ == "player" then "I use a microphone" else "Microphone required"
        , checked: context.draft.microphone
        , onChange: \checked -> context.onChange "mic" _ { microphone = checked }
        }
    ]

agesField :: ∀ m slots. MonadEffect m => Context m -> Html m slots
agesField context =
    field (shell context "ages" "Ages" Group Nothing)
    [ ageRange
        { from: maybe "" show context.draft.ageFrom
        , to: maybe "" show context.draft.ageTo
        , onFrom: \value -> context.onChange "ages" _ { ageFrom = Int.fromString value }
        , onTo: \value -> context.onChange "ages" _ { ageTo = Int.fromString value }
        }
    ]

timeFields :: ∀ m slots. MonadEffect m => Context m -> Array (Html m slots)
timeFields context@{ draft } =
    [ field (shell context "hours" "Usually online" Group (Just hoursHint))
        [ hoursRange
            { from: fromMaybe "" draft.online.from
            , to: fromMaybe "" draft.online.to
            , onFrom: \value -> context.onChange "hours" _ { online { from = blank value } }
            , onTo: \value -> context.onChange "hours" _ { online { to = blank value } }
            }
        ]
    , accountField context Timezone "Timezone" For
        (Just "Your hours are in this timezone. Everyone else sees them in theirs.")
        (select [ HP.id $ idOf "timezone" ]
            { options: allTimezones <#> \{ name } -> { value: name, label: factText Timezone name }
            , value: fromMaybe context.timezone draft.timezone
            , placeholder: Nothing
            , onChange: \value -> context.onChange "timezone" _ { timezone = Just value }
            })
    ]
    where
    blank value = if value == "" then Nothing else Just value

-- | The post's fields, the game's among them, with nothing behind a click
-- | (brief 6, step 3).
cardFields :: ∀ m slots. MonadEffect m => Context m -> Array (Html m slots)
cardFields context = case context.type_ of
    "group" ->
        [ nameField context, sizeField context ]
        <> gameFields context
        <> [ regionsField context
           , field (shell context "languages" "Languages" Group Nothing) [ languagesControl context ]
           , microphoneField context
           , agesField context
           ]
        <> timeFields context
    "community" ->
        [ nameField context ]
        <> gameFields context
        <> [ regionsField context
           , field (shell context "languages" "Languages" Group Nothing) [ languagesControl context ]
           , agesField context
           , microphoneField context
           ]
        <> timeFields context
    _ ->
        gameFields context
        <> [ accountField context Location "Location" For Nothing
                (select [ HP.id $ idOf "location" ]
                    { options: context.countries <#> \country -> { value: country, label: country }
                    , value: fromMaybe "" context.draft.country
                    , placeholder: Just "Choose a country"
                    , onChange: \value -> context.onChange "location" _
                        { country = if value == "" then Nothing else Just value }
                    })
           , accountField context Languages "Languages" Group Nothing (languagesControl context)
           , accountField context Birthday "Birthday" For (Just "Your card shows your age, never your birthday.")
                (input [ HP.id $ idOf "birthday", HP.type_ HP.InputDate ]
                    { value: fromMaybe "" context.draft.birthday
                    , placeholder: ""
                    , onInput: \value -> context.onChange "birthday" _
                        { birthday = if value == "" then Nothing else Just value }
                    })
           , microphoneField context
           ]
        <> timeFields context

wordsText :: String -> { label :: String, ideas :: String, placeholder :: String }
wordsText = case _ of
    "group" ->
        { label: "Tell people about your group"
        , ideas: "How do you play? What are you aiming for this season?"
        , placeholder: "Three friends who play most nights. We want to stop solo queuing for the last two spots."
        }
    "community" ->
        { label: "Tell people about your community"
        , ideas: "What do members do together? How big are you, and what are the rules?"
        , placeholder: "Weekly events, a friendly Discord and admins online most evenings. New members get a guide to get started."
        }
    _ ->
        { label: "About you and what you're looking for"
        , ideas: "How do you play? What are you aiming for? When are you usually on?"
        , placeholder: "Peak Diamond, back after a break. Looking for a chill duo for ranked in the evenings."
        }

-- | The player's own words, required only of a community, where the text is
-- | the product.
wordsField :: ∀ m slots. MonadEffect m => Context m -> Html m slots
wordsField context = let
    words = wordsText context.type_
    in
    field ((shell context "text" words.label For (Just $ "Ideas: " <> words.ideas))
        { required = context.type_ == "community" })
    [ textarea [ HP.id $ idOf "text" ]
        { value: context.draft.text
        , placeholder: words.placeholder
        , onInput: \value -> context.onChange "text" _ { text = value }
        }
    ]

-- | The keys of the fields that say how the post is reached, which settle one
-- | another's errors.
contactKeys :: ViewGame.OkContent -> Array String
contactKeys game = [ "reach", "discordServer", "website" ] <> game.contacts

linkField :: ∀ m slots. MonadEffect m => Context m -> String -> String -> String -> (Draft -> String) -> (String -> Draft -> Draft) -> Html m slots
linkField context key label placeholder get set =
    field (shell context key label For Nothing)
    [ input [ HP.id $ idOf key ]
        { value: get context.draft, placeholder, onInput: \value -> context.onChange key $ set value }
    ]

-- | How people reach the post or join the community (brief 6, step 3), and
-- | the game's contacts, which live on the account.
contactFields :: ∀ m slots. MonadEffect m => Context m -> Array (Html m slots)
contactFields context@{ game, type_ } =
    if type_ == "community"
    then
        [ field (shell context "reach" "How do people join?" Group (Just "Players can always message you here too."))
            [ choiceList
                { id: idOf "reach"
                , options:
                    [ { value: "discord", label: "They join our Discord server" }
                    , { value: "website", label: "They apply on our website" }
                    , { value: "message", label: "They message me first" }
                    ]
                , chosen: context.draft.reach
                , onChoose: \value -> context.onChange "reach" _ { reach = value }
                }
            ]
        , linkField context "discordServer" "Discord invite" "discord.gg/…" _.discordServer \value -> _ { discordServer = value }
        , linkField context "website" "Website" "https://…" _.website \value -> _ { website = value }
        ]
    else
        [ field
            (shell context "reach" "How should people reach you?" Group
                (Just "People can always message you here too, so your contacts are optional."))
            [ choiceList
                { id: idOf "reach"
                , options:
                    [ { value: "message", label: "Message me on TeamTavern" }
                    , { value: "offsite", label: "Add me on Discord or in game" }
                    , { value: "either", label: "Either is fine" }
                    ]
                , chosen: context.draft.reach
                , onChoose: \value -> context.onChange "reach" _ { reach = value }
                }
            ]
        ]
        <> (kinds <#> contactField)
        <> if type_ == "group"
            then
                [ linkField context "discordServer" "Discord server" "discord.gg/…" _.discordServer \value -> _ { discordServer = value }
                , linkField context "website" "Website" "https://…" _.website \value -> _ { website = value }
                ]
            else []
    where
    -- Discord first, then the game's accounts.
    kinds = game.contacts # sortBy \a b -> compare (a /= "discord") (b /= "discord") <> compare a b
    trackersOf kind = game.trackers # filter (_.contact >>> eq kind) <#> _.title
    trackerHint kind = case trackersOf kind of
        [] -> Nothing
        titles | type_ == "player" -> Just $ "Your card links your " <> joinWith " and " titles <> " profile from it."
        _ -> Nothing
    contactInput kind =
        input [ HP.id $ idOf kind ]
            { value: Object.lookup kind context.draft.contacts # fromMaybe ""
            , placeholder: contactPlaceholder kind
            , onInput: \value -> context.onChange kind \draft -> draft { contacts = Object.insert kind value draft.contacts }
            }
    contactField kind =
        accountField context (Contact kind) (contactLabel kind) For (trackerHint kind)
        if kind == "discord" && isNothing context.account
        then HH.div [ HS.class_ "input-row" ]
            [ contactInput kind
            , button Outline Regular context.onDiscord [ Icons.discord, HH.text "Sign up with Discord" ]
            ]
        else contactInput kind

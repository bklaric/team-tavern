module TeamTavern.Client.Pages.Feed.Fields
    ( BarField
    , Kind
    , Lists
    , barFields
    , clear
    , editor
    , isToggle
    , summary
    , toggle
    ) where

import Prelude

import Data.Array (delete, elem, filter, find, length, null, snoc, take)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (joinWith, toLower)
import Data.String.CodeUnits as CodeUnits
import Foreign.Object as Object
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import TeamTavern.Client.Components.Button (Size(..), Weight(..), button)
import TeamTavern.Client.Components.Card (flagText)
import TeamTavern.Client.Components.Card.Hours (hoursText, toMinutes)
import TeamTavern.Client.Components.Card.Regions (regionCount, regionShortName)
import TeamTavern.Client.Components.Input (Option, input, select)
import TeamTavern.Client.Components.Range (ageRange, hoursHint, hoursRange, optionRange)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Game.ViewGame as ViewGame
import TeamTavern.Routes.Shared.Description (Description)
import TeamTavern.Routes.Shared.Field (Field)
import TeamTavern.Shared.Languages (languageCode)

-- What a field of the bar holds and how it is edited. A game field is keyed
-- by its own key: a player gives a point on an ordered field, a group or a
-- community a range, and a boolean is a toggle.
data Kind
    = Choose { multiple :: Boolean, all :: Maybe String }
    | Point
    | Between
    | Flag
    | Location
    | Age
    | Regions
    | Languages
    | Ages
    | Online
    | Microphone

type BarField =
    { key :: String
    , label :: String
    , kind :: Kind
    , options :: Array Option
    -- The sentence a toggle reads as in the phone's sheet.
    , toggleLabel :: String
    , more :: Boolean
    }

-- | The lists the account's facts are chosen from, languages most used first.
type Lists = { countries :: Array String, regions :: Array String, languages :: Array String }

optionsOf :: Field -> Array Option
optionsOf field = field.options <#> \{ key, label } -> { value: key, label }

article :: String -> String
article word = if elem (CodeUnits.take 1 $ toLower word) [ "a", "e", "i", "o", "u" ] then "an" else "a"

gameField :: String -> Field -> BarField
gameField type_ field
    | field.ilk == "boolean" =
        { key: field.key
        , label: flagText type_ field true
        , kind: Flag
        , options: []
        , toggleLabel:
            if type_ == "player"
            then "I can be " <> article field.label <> " " <> toLower field.label
            else flagText type_ field true
        , more: not field.onCard
        }
    | field.ordered && type_ == "player" =
        { key: field.key, label: field.label, kind: Point, options: optionsOf field, toggleLabel: "", more: not field.onCard }
    | field.ordered =
        { key: field.key, label: field.label <> " range", kind: Between, options: optionsOf field, toggleLabel: "", more: not field.onCard }
    | otherwise =
        { key: field.key
        , label: field.label
        , kind: Choose
            { multiple: field.ilk == "multi"
            , all: if field.slotted then Just ("Any " <> toLower field.label) else Nothing
            }
        , options: optionsOf field
        , toggleLabel: ""
        , more: not field.onCard
        }

fact :: String -> String -> Kind -> Array Option -> BarField
fact key label kind options = { key, label, kind, options, toggleLabel: "", more: false }

-- | The fields the bar carries for a type: only those matching compares
-- | (brief 7.1). The game's fields that lead the card and the account's facts
-- | come first, and the rest under More.
barFields :: ViewGame.OkContent -> Lists -> String -> Array BarField
barFields game lists type_ =
    filter (not <<< _.more) gameFields <> facts <> filter _.more gameFields <> [ hours, mic ]
    where
    gameFields = game.fields # filter (_.appliesTo >>> elem type_) <#> gameField type_
    languages = fact "languages" "Languages" Languages (lists.languages <#> \language -> { value: language, label: language })
    regions = fact "regions" "Regions" Regions (lists.regions <#> \region -> { value: region, label: region })
    facts =
        if type_ == "player"
        then
            [ fact "location" "Location" Location (lists.countries <#> \country -> { value: country, label: country })
            , languages
            , fact "age" "Age" Age []
            ]
        else [ regions, languages, fact "ages" "Ages" Ages [] ]
    hours = (fact "hours" "Usually online" Online []) { more = true }
    mic = (fact "mic" "Microphone" Microphone [])
        { more = true
        , toggleLabel = if type_ == "player" then "I use a microphone" else "Microphone required"
        }

isToggle :: BarField -> Boolean
isToggle field = case field.kind of
    Flag -> true
    Microphone -> true
    _ -> false

labelOf :: BarField -> String -> String
labelOf field value = find (_.value >>> eq value) field.options <#> _.label # fromMaybe value

shortList :: Array String -> String
shortList labels
    | length labels > 2 = joinWith ", " (take 2 labels) <> " +" <> show (length labels - 2)
    | otherwise = joinWith ", " labels

chosen :: BarField -> Description -> Array String
chosen field description = Object.lookup field.key description.options # fromMaybe []

-- | What the field's chip reads when it holds something.
summary :: BarField -> Description -> Maybe String
summary field description = case field.kind of
    Choose { all } -> case chosen field description of
        [] -> Nothing
        values
            | isJust all && length values == length field.options -> all
            | otherwise -> Just $ shortList $ values <#> labelOf field
    Point -> case chosen field description of
        [ value ] -> Just $ labelOf field value
        _ -> Nothing
    Between -> Object.lookup field.key description.ranges >>= case _ of
        { from: Just from, to: Just to }
            | from == to -> Just $ labelOf field from
            | otherwise -> Just $ labelOf field from <> " – " <> labelOf field to
        { from: Just from } -> Just $ labelOf field from <> " and up"
        { to: Just to } -> Just $ "Up to " <> labelOf field to
        _ -> Nothing
    Flag -> if elem field.key description.flags then Just field.label else Nothing
    Location -> description.country
    Age -> description.age <#> \age -> "Age " <> show age
    Regions -> case description.regions of
        [] -> Nothing
        regions
            | length regions == regionCount -> Just "Anywhere"
            | otherwise -> Just $ shortList $ regions <#> regionShortName
    Languages -> case description.languages of
        [] -> Nothing
        languages -> Just $ joinWith ", " $ languages <#> languageCode
    Ages -> case description.ageFrom, description.ageTo of
        Just from, Just to -> Just $ "Ages " <> show from <> "–" <> show to
        Just from, Nothing -> Just $ "Ages " <> show from <> "+"
        Nothing, Just to -> Just $ "Ages up to " <> show to
        Nothing, Nothing -> Nothing
    Online -> do
        { from, to } <- description.online
        from' <- from >>= toMinutes
        to' <- to >>= toMinutes
        pure $ hoursText { from: from', to: to' }
    Microphone -> if description.microphone then Just field.label else Nothing

toggled :: String -> Array String -> Array String
toggled value values = if elem value values then delete value values else snoc values value

-- | A toggle's chip flips it.
toggle :: BarField -> Description -> Description
toggle field description = case field.kind of
    Flag -> description { flags = toggled field.key description.flags }
    Microphone -> description { microphone = not description.microphone }
    _ -> description

clear :: BarField -> Description -> Description
clear field description = case field.kind of
    Choose _ -> withoutOption
    Point -> withoutOption
    Between -> description { ranges = Object.delete field.key description.ranges }
    Flag -> description { flags = delete field.key description.flags }
    Location -> description { country = Nothing }
    Age -> description { age = Nothing }
    Regions -> description { regions = [] }
    Languages -> description { languages = [] }
    Ages -> description { ageFrom = Nothing, ageTo = Nothing }
    Online -> description { online = Nothing, timezone = Nothing }
    Microphone -> description { microphone = false }
    where
    withoutOption = description { options = Object.delete field.key description.options }

-- | One field's control, in a desktop popover or the phone's sheet. `id`
-- | keeps its inputs' names apart from the other place the field is edited.
editor :: ∀ w i. String -> BarField -> Description -> (Description -> i) -> HH.HTML w i
editor id field description onChange =
    HH.div [ HS.class_ "editor" ] case field.kind of
    Choose { multiple, all } ->
        [ options multiple (chosen field description) \value ->
            setOptions if multiple then toggled value (chosen field description) else [ value ]
        ]
        <> maybe []
            (\label -> [ button Text Small (setOptions $ field.options <#> _.value) [ HH.text label ] ])
            all
    Point ->
        [ select [ HPA.label field.label ]
            { options: field.options
            , value: chosen field description # take 1 # joinWith ""
            , placeholder: Just $ "Choose your " <> toLower field.label
            , onChange: \value -> setOptions if value == "" then [] else [ value ]
            }
        ]
    Between ->
        let range = Object.lookup field.key description.ranges # fromMaybe { from: Nothing, to: Nothing }
            setRange range' = onChange description { ranges = Object.insert field.key range' description.ranges }
        in
        [ optionRange field.options
            { name: field.label
            , from: fromMaybe "" range.from
            , to: fromMaybe "" range.to
            , onFrom: \value -> setRange range { from = blank value }
            , onTo: \value -> setRange range { to = blank value }
            }
        ]
    Location ->
        [ select [ HPA.label field.label ]
            { options: field.options
            , value: fromMaybe "" description.country
            , placeholder: Just "Choose a country"
            , onChange: \value -> onChange description { country = blank value }
            }
        ]
    Age ->
        [ input
            [ HP.type_ HP.InputNumber
            , HP.min 13.0
            , HP.max 99.0
            , HP.attr (HH.AttrName "inputmode") "numeric"
            , HPA.label "Your age"
            ]
            { value: maybe "" show description.age
            , placeholder: "Your age"
            , onInput: \value -> onChange description { age = Int.fromString value }
            }
        ]
    Regions ->
        [ options true description.regions \value ->
            onChange description { regions = toggled value description.regions }
        ]
    Languages ->
        [ options true description.languages \value ->
            onChange description { languages = toggled value description.languages }
        ]
    Ages ->
        [ ageRange
            { name: field.label
            , from: maybe "" show description.ageFrom
            , to: maybe "" show description.ageTo
            , onFrom: \value -> onChange description { ageFrom = Int.fromString value }
            , onTo: \value -> onChange description { ageTo = Int.fromString value }
            }
        ]
    Online ->
        let online = description.online # fromMaybe { from: Nothing, to: Nothing }
            -- Hours given here are in the viewer's own timezone.
            setOnline online' = onChange description { online = Just online', timezone = Nothing }
        in
        [ hoursRange
            { name: field.label
            , from: fromMaybe "" online.from
            , to: fromMaybe "" online.to
            , onFrom: \value -> setOnline online { from = blank value }
            , onTo: \value -> setOnline online { to = blank value }
            }
        , HH.span [ HS.class_ "editor-hint" ] [ HH.text hoursHint ]
        ]
    Flag -> [ check $ elem field.key description.flags ]
    Microphone -> [ check description.microphone ]
    where
    name = id <> "-" <> field.key
    blank value = if value == "" then Nothing else Just value
    setOptions values = onChange description
        { options = if null values
            then Object.delete field.key description.options
            else Object.insert field.key values description.options
        }
    options multiple values onToggle =
        HH.div [ HS.class_ "options", HPA.role if multiple then "group" else "radiogroup", HPA.label field.label ] $
        field.options <#> \option ->
            HH.label [ HS.class_ "option" ]
            [ HH.input
                [ HP.type_ if multiple then HP.InputCheckbox else HP.InputRadio
                , HP.name name
                , HP.value option.value
                , HP.checked $ elem option.value values
                , HE.onChange $ const $ onToggle option.value
                ]
            , HH.text option.label
            ]
    check checked =
        HH.label [ HS.class_ "option" ]
        [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked checked
            , HE.onChange $ const $ onChange $ toggle field description
            ]
        , HH.text field.toggleLabel
        ]

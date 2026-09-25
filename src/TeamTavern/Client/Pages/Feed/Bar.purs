module TeamTavern.Client.Pages.Feed.Bar (bar, sheet, summaryButton) where

import Prelude

import Data.Array (any, filter, mapMaybe, mapWithIndex, null)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (joinWith)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import TeamTavern.Client.Components.Button (Size(..), Weight(..), button)
import TeamTavern.Client.Components.Card (typeIcon)
import TeamTavern.Client.Components.Check (choices)
import TeamTavern.Client.Components.Overlay (Presentation(..), overlay, overlayId)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Pages.Feed.Description (isEmpty)
import TeamTavern.Client.Pages.Feed.Fields (BarField, clear, editor, isToggle, summary, toggle)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Shared.Description (Description)

-- A player fills the fields in about themselves; a group or a community fills
-- them in about the players it wants. It is the inversion the cards carry
-- (brief 5), and the bar is where the viewer first meets it.
reading :: String -> String
reading "group" = "Fill these in about the players you want."
reading "community" = "Fill these in about the members you want."
reading _ = "Fill these in about yourself."

typeChoice :: ∀ w i. String -> String -> (String -> i) -> HH.HTML w i
typeChoice name chosen onChoose =
    HH.div [ HS.class_ "type-choice" ]
    [ choices
        { name
        , label: "You are"
        , options:
            [ { value: "player", label: "I'm a player looking for a group", icon: typeIcon "player" }
            , { value: "group", label: "We're a group looking for players", icon: typeIcon "group" }
            , { value: "community", label: "We're a community looking for members", icon: typeIcon "community" }
            ]
        , chosen
        , onChoose
        }
    , HH.p [ HS.class_ "description-reading" ] [ HH.text $ reading chosen ]
    ]

type Bar i =
    { ref :: H.RefLabel
    , fields :: Array BarField
    , description :: Description
    , openField :: Maybe String
    , showMore :: Boolean
    , onType :: String -> i
    , onChange :: Description -> i
    , onOpen :: Maybe String -> i
    , onMore :: i
    , onClearAll :: i
    }

-- | The description on a desktop: the type, then a chip per field that opens
-- | its editor in a popover. The feed follows every change.
bar :: ∀ w i. Bar i -> HH.HTML w i
bar { ref, fields, description, openField, showMore, onType, onChange, onOpen, onMore, onClearAll } =
    HH.section [ HS.class_ "description", HPA.labelledBy "description-heading" ]
    [ HH.div [ HS.class_ "description-heading" ]
        [ HH.h2 [ HP.id "description-heading" ] [ HH.text "Find posts that fit you" ]
        , HH.p_ [ HH.text "Tell us about you, and posts that fit come first." ]
        ]
    , typeChoice "bar-type" description.type onType
    , HH.div [ HS.class_ "field-chips" ] $
        (primary <#> chip [])
        <> mapWithIndex (\index -> chip if index == 0 then [ HP.attr (HH.AttrName "data-first-more") "" ] else [])
            shownMore
        <> (if null more || not (null shownMore) then []
            else [ HH.button [ HS.class_ "field-chip", HP.type_ HP.ButtonButton, HE.onClick $ const onMore ]
                [ HH.text "More", Icons.chevronDown ] ])
        <> (if isEmpty description then []
            else [ button Text Small onClearAll [ HH.text "Clear all" ] ])
    ]
    where
    primary = filter (not <<< _.more) fields
    more = filter _.more fields
    -- A field under More that holds something stays in view.
    shownMore = if showMore || any (\field -> isJust $ summary field description) more then more else []
    -- The first chip More shows is marked, so the focus can go to it.
    chip marks field
        | isToggle field =
            let on = isJust $ summary field description in
            HH.div [ HS.class_ "field-chip-wrap" ]
            [ HH.button
                ( [ HS.class_ $ "field-chip" <> if on then " field-chip-filled" else ""
                  , HP.type_ HP.ButtonButton
                  , HPA.pressed $ show on
                  , HE.onClick $ const $ onChange $ toggleOf field
                  ]
                  <> marks
                )
                [ HH.text field.label ]
            ]
        | otherwise =
            let text = summary field description
                open = openField == Just field.key
            in
            HH.div [ HS.class_ "field-chip-wrap" ]
            [ HH.button
                ( [ HS.class_ $ "field-chip" <> if isJust text then " field-chip-filled" else ""
                  , HP.type_ HP.ButtonButton
                  , HPA.expanded $ show open
                  , HPA.controls $ overlayId ref
                  , HPA.hasPopup "dialog"
                  -- A filled chip reads what it holds, so it names its field too.
                  , HPA.label $ maybe field.label (\text' -> field.label <> ": " <> text') text
                  , HE.onClick $ const $ onOpen if open then Nothing else Just field.key
                  ]
                  <> marks
                )
                [ HH.text $ fromMaybe field.label text, Icons.chevronDown ]
            , if open
                then overlay
                    { ref
                    , presentation: Dropdown { className: "popover", role: "dialog" }
                    , title: field.label
                    , onClose: onOpen Nothing
                    }
                    [ editor "bar" field description onChange
                    , HH.div [ HS.class_ "popover-footer" ]
                        [ button Text Small (onChange $ clearOf field) [ HH.text "Clear" ]
                        , button Outline Small (onOpen Nothing) [ HH.text "Done" ]
                        ]
                    ]
                    []
                else HH.text ""
            ]
    toggleOf field = toggle field description
    clearOf field = clear field description

-- | The description on a phone: a summary that opens the full-screen sheet.
summaryButton :: ∀ w i. { fields :: Array BarField, description :: Description, onOpen :: i } -> HH.HTML w i
summaryButton { fields, description, onOpen } =
    HH.button
    [ HS.class_ "description-summary", HP.type_ HP.ButtonButton, HPA.hasPopup "dialog", HE.onClick $ const onOpen ]
    [ typeIcon description.type
    , HH.span_ [ HH.text text ]
    , Icons.pencil
    ]
    where
    text = case mapMaybe (flip summary description) fields of
        [] -> "Tell us about you to see what fits"
        parts -> joinWith " · " parts

-- | Every field at once, filled in on a phone. The feed updates when the sheet
-- | closes (brief 7.1).
sheet :: ∀ w i.
    { ref :: H.RefLabel
    , fields :: Array BarField
    , description :: Description
    , onType :: String -> i
    , onChange :: Description -> i
    , onClose :: i
    }
    -> HH.HTML w i
sheet { ref, fields, description, onType, onChange, onClose } =
    overlay { ref, presentation: FullScreen, title: "Tell us about you", onClose }
    ( [ typeChoice "sheet-type" description.type onType ]
    <> (fields <#> \field ->
        HH.div [ HS.class_ "sheet-field" ]
        [ HH.h3_ [ HH.text field.label ]
        , editor "sheet" field description onChange
        ])
    )
    [ button Primary Regular onClose [ HH.text "Show posts" ] ]

module TeamTavern.Client.Components.Field
    ( Field
    , Labelling(..)
    , field
    , field_
    , formSection
    , labelId
    ) where

import Prelude

import Data.Array (catMaybes)
import Data.Maybe (Maybe(..), isJust, maybe)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Snippets.Class as HS

-- How the label names its control. A single control has the field's id and is
-- named by `for`; a group of pills or radios names the label by its id through
-- aria-labelledby; a check or a switch carries its own sentence and has no
-- label above it.
data Labelling = For | Group | Unlabelled

type Field =
    { id :: String
    , label :: String
    , labelling :: Labelling
    , required :: Boolean
    , hint :: Maybe String
    , note :: Maybe String
    , error :: Maybe String
    }

-- A field of a single control, with nothing below it; update the record for
-- the rest.
field_ :: String -> String -> Field
field_ id label =
    { id, label, labelling: For, required: false, hint: Nothing, note: Nothing, error: Nothing }

labelId :: String -> String
labelId id = "l-" <> id

-- The label over the control, then the hint, a note and an error.
field :: ∀ w i. Field -> Array (HH.HTML w i) -> HH.HTML w i
field { id, label, labelling, required, hint, note, error } control =
    HH.div [ HS.class_ $ "field" <> if isJust error then " field-invalid" else "" ] $
    catMaybes [ labelHtml ]
    <> control
    <> catMaybes
        [ hint <#> \hint' -> HH.span [ HS.class_ "field-hint" ] [ HH.text hint' ]
        , note <#> \note' -> HH.span [ HS.class_ "field-note" ] [ Icons.info, HH.text note' ]
        , error <#> \error' ->
            HH.span [ HS.class_ "field-error", HPA.role "alert" ] [ Icons.circleAlert, HH.text error' ]
        ]
    where
    labelText = [ HH.text label ] <> if required then [ HH.span [ HS.class_ "field-tag" ] [ HH.text "Required" ] ] else []
    labelHtml = case labelling of
        For -> Just $ HH.label [ HS.class_ "field-label", HP.id $ labelId id, HP.for id ] labelText
        Group -> Just $ HH.span [ HS.class_ "field-label", HP.id $ labelId id ] labelText
        Unlabelled -> Nothing

-- Fields stacked in a section, under a heading if it has one.
formSection :: ∀ w i. Maybe String -> Array (HH.HTML w i) -> HH.HTML w i
formSection heading fields =
    HH.div [ HS.class_ "form-section" ] $
    maybe [] (\heading' -> [ HH.h2_ [ HH.text heading' ] ]) heading <> fields

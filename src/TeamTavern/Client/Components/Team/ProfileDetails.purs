module TeamTavern.Client.Components.Team.ProfileDetails where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Record.Extra (pick)
import TeamTavern.Client.Components.Detail (detail, fieldDetail)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Shared.Field (Fields, ValuesMulti, ValuesSimpleMulti)

profileDetails :: ∀ slots action.
    Fields -> ValuesSimpleMulti -> Boolean -> Array (HH.HTML slots action)
profileDetails fields fieldValues newOrReturning =
    profileDetails'
    (fieldValues <#> (\fieldValue ->
        case fields # Array.find \{ key } -> key == fieldValue.fieldKey of
        Just field -> Just
            { field: pick field
            , options: field.options # Array.filter \{ key } -> Array.elem key fieldValue.optionKeys
            }
        Nothing -> Nothing
        )
        # Array.catMaybes
    )
    newOrReturning


profileDetails' :: ∀ slots action.
    ValuesMulti -> Boolean -> Array (HH.HTML slots action)
profileDetails' fieldValues newOrReturning =
    ( fieldValues
    <#> ( \fieldValue ->
            if not $ Array.null fieldValue.options
            then Just $
                fieldDetail fieldValue.field.icon fieldValue.field.label
                ( Array.intercalate [(HH.text ", ")] $
                    map
                    (\{ label } ->
                        [ HH.span [ HS.class_ "detail-emphasize" ]
                            [ HH.text label ]
                        ])
                    fieldValue.options
                )
            else Nothing
        )
    # Array.catMaybes
    )
    <>
    ( if newOrReturning
        then Array.singleton $
            detail "fas fa-book"
            [ HH.span [ HS.class_ "detail-labelless" ] [ HH.text "Are "]
            , HH.span [ HS.class_ "detail-emphasize" ] [ HH.text "new or returning players" ]
            , HH.text $ " to the game"
            ]
        else []
    )

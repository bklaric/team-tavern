module TeamTavern.Client.Components.Team.ProfileDetails where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import TeamTavern.Client.Snippets.Class as HS

profileDetails :: forall fieldOptionFields fieldValueFields fieldFields slots action.
    Array
        { icon :: String
        , key :: String
        , label :: String
        , options :: Array
            { key :: String
            , label :: String
            | fieldOptionFields
            }
        | fieldFields
        }
    -> Array
        { fieldKey :: String
        , optionKeys :: Array String
        | fieldValueFields
        }
    -> Boolean
    -> Array (HH.HTML slots action)
profileDetails fields fieldValues newOrReturning =
    profileDetails'
    ( fieldValues <#> (\fieldValue ->
        case fields # Array.find \{ key } -> key == fieldValue.fieldKey of
        Just field -> Just
            { field:
                { icon: field.icon
                , key: field.key
                , label: field.label
                }
            , options: field.options # Array.filter \{ key } -> Array.elem key fieldValue.optionKeys
            }
        Nothing -> Nothing
        )
    # Array.catMaybes
    )
    newOrReturning

profileDetails' :: forall fieldOptionFields fieldFields slots action.
    Array
        { field ::
            { key :: String
            , label :: String
            , icon :: String
            | fieldFields
            }
        , options :: Array
            { key :: String
            , label :: String
            | fieldOptionFields
            }
        }
    -> Boolean
    -> Array (HH.HTML slots action)
profileDetails' fieldValues newOrReturning =
    ( fieldValues
    <#> ( \fieldValue ->
            if not $ Array.null fieldValue.options
            then Just $
                HH.p [ HS.class_ "detail" ] $
                [ HH.i [ HS.class_ $ fieldValue.field.icon <> " detail-icon" ] []
                , HH.span [ HS.class_ "detail-label" ]
                    [ HH.text $ fieldValue.field.label <> ": " ]
                ]
                <>
                ( Array.intercalate [(HH.text ", ")] $
                    map
                    (\{ label } ->
                        [ HH.span [ HS.class_ "detail-emphasize" ]
                            [ HH.text label ]
                        ])
                    fieldValue.options
                )
            else Nothing
        -- _ -> Nothing
        )
    # Array.catMaybes
    )
    <>
    ( if newOrReturning
        then Array.singleton $
            HH.p [ HS.class_ "detail" ]
            [ HH.i [ HS.class_ "fas fa-book detail-icon" ] []
            , HH.span [ HS.class_ "detail-labelless" ] [ HH.text "Are"]
            , HH.span [ HS.class_ "detail-emphasize" ] [ HH.text " new or returning players" ]
            , HH.text $ " to the game"
            ]
        else []
    )

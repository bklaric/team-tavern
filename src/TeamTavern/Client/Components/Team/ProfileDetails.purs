module TeamTavern.Client.Components.Team.ProfileDetails where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import TeamTavern.Client.Components.Detail (detail, fieldDetail)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Shared.Platform (Platform, Platforms)

profileDetails :: forall fieldOptionFields fieldValueFields fieldFields someMoreFields slots action.
    { allPlatforms :: Platforms
    , selectedPlatforms :: Array Platform
    , fields :: Array
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
    , fieldValues ::  Array
        { fieldKey :: String
        , optionKeys :: Array String
        | fieldValueFields
        }
    , newOrReturning :: Boolean
    | someMoreFields
    }
    -> Array (HH.HTML slots action)
profileDetails { allPlatforms, selectedPlatforms, fields, fieldValues, newOrReturning } =
    profileDetails'
    { allPlatforms
    , selectedPlatforms
    , fieldValues: fieldValues <#> (\fieldValue ->
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
    , newOrReturning
    }

profileDetails' :: forall fieldOptionFields fieldFields someMoreFields slots action.
    { fieldValues :: Array
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
    , newOrReturning :: Boolean
    | someMoreFields
    }
    -> Array (HH.HTML slots action)
profileDetails' { fieldValues, newOrReturning } =
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

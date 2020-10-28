module TeamTavern.Client.Components.Player.ProfileDetails where

import Prelude

import Data.Array (intercalate)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import TeamTavern.Client.Components.Detail (detail, fieldDetail, urlDetail)
import TeamTavern.Client.Snippets.Class as HS

profileDetails :: forall slots action.
    Array
        { ilk :: Int
        , key :: String
        , label :: String
        , icon :: String
        , required :: Boolean
        , domain :: Maybe String
        , options :: Maybe (Array
            { key :: String
            , label :: String
            })
        }
    -> Array
        { fieldKey :: String
        , url :: Maybe String
        , optionKey :: Maybe String
        , optionKeys :: Maybe (Array String)
        }
    -> Boolean
    -> Array (HH.HTML slots action)
profileDetails fields fieldValues newOrReturning =
    profileDetails'
    ( fields
    <#> ( \field ->
            case fieldValues # Array.find \{ fieldKey } -> fieldKey == field.key of
            Just fieldValue -> Just
                { field:
                    { ilk: field.ilk
                    , icon: field.icon
                    , key: field.key
                    , label: field.label
                    }
                , url: fieldValue.url
                , option:
                    case field.options, fieldValue.optionKey of
                    Just options, Just optionKey ->
                        options # Array.find \option -> option.key == optionKey
                    _, _ -> Nothing
                , options:
                    case field.options, fieldValue.optionKeys of
                    Just options, Just optionKeys ->
                        options # Array.filter (\option -> Array.elem option.key optionKeys) # Just
                    _, _ -> Nothing
                }
            Nothing -> Nothing
        )
    # Array.catMaybes
    )
    newOrReturning

profileDetails' :: forall slots action.
    Array
        { field ::
            { ilk :: Int
            , key :: String
            , label :: String
            , icon :: String
            }
        , url :: Maybe String
        , option :: Maybe
            { key :: String
            , label :: String
            }
        , options :: Maybe (Array
            { key :: String
            , label :: String
            })
        }
    -> Boolean
    -> Array (HH.HTML slots action)
profileDetails' fieldValues newOrReturning =
    ( fieldValues
    <#> ( \{ field, url, option, options } ->
            case field.ilk, url, option, options of
            1, Just url', _, _ -> urlDetail field.icon field.label (Just url')
            2, _, Just option', _ -> Just $
                fieldDetail field.icon field.label
                [ HH.span [ HS.class_ "detail-emphasize" ] [ HH.text option'.label ] ]
            3, _, _, Just options' | not $ Array.null options' -> Just $
                fieldDetail field.icon field.label
                ( intercalate [ HH.text ", " ] $ map
                    ( \{ label } ->
                        [ HH.span [ HS.class_ "detail-emphasize" ] [ HH.text label ] ]
                    )
                    options'
                )
            _, _, _, _ ->  Nothing
        )
    # Array.catMaybes
    )
    <>
    ( if newOrReturning
        then Array.singleton $
            detail "fas fa-book"
            [ HH.span [ HS.class_ "detail-labelless" ] [ HH.text "Are"]
            , HH.span [ HS.class_ "detail-emphasize" ] [ HH.text " new or returning players" ]
            , HH.text $ " to the game"
            ]
        else []
    )
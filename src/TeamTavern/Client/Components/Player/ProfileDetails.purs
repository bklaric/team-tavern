module TeamTavern.Client.Components.Player.ProfileDetails where

import Prelude

import Data.Array (intercalate)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Record.Extra (pick)
import TeamTavern.Client.Components.Detail (detail, fieldDetail)
import TeamTavern.Client.Shared.Slot (Slot__String)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Shared.Field (Fields, Values, ValuesSimple)

type PlatformIdSlots slots =
    ( steamId :: Slot__String
    , riotId :: Slot__String
    , battleTag :: Slot__String
    , eaId :: Slot__String
    , ubisoftUsername :: Slot__String
    , psnId :: Slot__String
    , gamerTag :: Slot__String
    , friendCode :: Slot__String
    | slots )

profileDetails :: ∀ slots action.
    Fields -> ValuesSimple -> Boolean -> Array (HH.HTML slots action)
profileDetails fields fieldValues newOrReturning =
    profileDetails'
    (fields <#> (\field ->
        case fieldValues # Array.find \{ fieldKey } -> fieldKey == field.key of
        Just fieldValue -> Just
            { field: pick field
            , option: fieldValue.optionKey >>= \optionKey ->
                field.options # Array.find \option -> option.key == optionKey
            , options: fieldValue.optionKeys <#> \optionKeys ->
                field.options # Array.filter (\option -> Array.elem option.key optionKeys)
            }
        Nothing -> Nothing
        )
        # Array.catMaybes
    )
    newOrReturning

profileDetails' :: ∀ slots action.
    Values -> Boolean -> Array (HH.HTML slots action)
profileDetails' fieldValues newOrReturning =
    ( fieldValues
    <#> ( \{ field, option, options } ->
            case field.ilk, option, options of
            "single", Just option', _ -> Just $
                fieldDetail field.icon field.label
                [ HH.span [ HS.class_ "detail-emphasize" ] [ HH.text option'.label ] ]
            "multi", _, Just options' | not $ Array.null options' -> Just $
                fieldDetail field.icon field.label
                ( intercalate [ HH.text ", " ] $ map
                    ( \{ label } ->
                        [ HH.span [ HS.class_ "detail-emphasize" ] [ HH.text label ] ]
                    )
                    options'
                )
            _, _, _ ->  Nothing
        )
    # Array.catMaybes
    )
    <>
    ( if newOrReturning
        then Array.singleton $
            detail "fas fa-book"
            [ HH.span [ HS.class_ "detail-labelless" ] [ HH.text "Is a "]
            , HH.span [ HS.class_ "detail-emphasize" ] [ HH.text "new or returning player" ]
            , HH.text $ " to the game"
            ]
        else []
    )

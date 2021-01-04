module TeamTavern.Client.Components.Detail where

import Prelude

import Async (Async)
import Client.Components.Copyable (copyable)
import Client.Components.Copyable as Copyable
import Data.Array as Array
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Brands (detailRiotSvg, detailSteamSvg)
import TeamTavern.Client.Snippets.Class as HS

detailColumnsContainer :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
detailColumnsContainer = HH.div [ HS.class_ "detail-columns-container" ]

detailColumns :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
detailColumns = HH.div [ HS.class_ "detail-columns" ]

detailColumn :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
detailColumn = HH.div [ HS.class_ "detail-column" ]

detailColumnHeading3 :: forall slots action. String -> HH.HTML slots action
detailColumnHeading3 heading = HH.h3 [ HS.class_ "detail-column-heading" ] [ HH.text heading ]

detailColumnHeading4 :: forall slots action. String -> HH.HTML slots action
detailColumnHeading4 heading = HH.h4 [ HS.class_ "detail-column-heading" ] [ HH.text heading ]

detail' :: forall slots action.
    HH.HTML slots action -> Array (HH.HTML slots action) -> HH.HTML slots action
detail' icon children = HH.p [ HS.class_ "detail" ] $ [ icon ] <> children

detail :: forall slots action. String -> Array (HH.HTML slots action) -> HH.HTML slots action
detail icon children = detail' (HH.i [ HS.class_ $ icon <> " detail-icon" ] []) children

steamUrlDetail :: forall slots action. String -> HH.HTML slots action
steamUrlDetail steamUrl =
    detail' detailSteamSvg
    [ HH.a
        [ HS.class_ "detail-url", HP.target "_blank", HP.href steamUrl ]
        [ HH.text "Steam profile" ]
    ]

riotIdDetail :: forall left slots action.
    String -> HH.ComponentHTML action (riotId :: Copyable.Slot String | slots) (Async left)
riotIdDetail riotId =
    detail' detailRiotSvg
    [ HH.span [ HS.class_ "detail-label" ] [ HH.text "Riot ID: " ]
    , HH.span [ HS.class_ "detail-emphasize" ] [ copyable (SProxy :: SProxy "riotId") riotId riotId ]
    ]

fieldDetail :: forall slots action.
    String -> String -> Array (HH.HTML slots action) -> HH.HTML slots action
fieldDetail icon label children =
    detail icon $
    [ HH.span [ HS.class_ "detail-label" ] [ HH.text $ label <> ": " ] ]
    <>
    children

urlDetail :: forall slots action. String -> String -> Maybe String -> Maybe (HH.HTML slots action)
urlDetail _ _ Nothing = Nothing
urlDetail icon text (Just href) = Just $
    detail icon
    [ HH.a
        [ HS.class_ "detail-url", HP.target "_blank", HP.href href ]
        [ HH.text text ]
    ]

arrangeItems :: forall slots action. String -> Array String -> Array (HH.HTML slots action)
arrangeItems binder items =
    foldr
    (\item state ->
        if not state.firstItem
        then state
            { firstItem = true
            , itemsSoFar = [ HH.span [ HS.class_ "detail-emphasize" ] [ HH.text item ] ]
            }
        else if not state.secondItem
        then state
            { secondItem = true
            , itemsSoFar =
                [ HH.span [ HS.class_ "detail-emphasize" ] [ HH.text item ]
                , HH.text $ " " <> binder <> " "
                ]
                <> state.itemsSoFar
            }
        else state
            { itemsSoFar =
                [ HH.span [ HS.class_ "detail-emphasize" ] [ HH.text item ]
                , HH.text ", "
                ]
                <> state.itemsSoFar
            }
    )
    { firstItem: false, secondItem: false, itemsSoFar: [] }
    items
    # _.itemsSoFar

arrangedDetail :: forall slots action.
    String -> String -> String -> Array String -> Maybe (HH.HTML slots action)
arrangedDetail _ _ _ items | Array.null items = Nothing
arrangedDetail binder icon prefix items = Just $
    detail icon $
    [ HH.span
        [ HS.class_ "detail-labelless" ]
        [ HH.text $ prefix <> " " ]
    ]
    <> arrangeItems binder items

arrangedOrDetail :: forall slots action.
    String -> String -> Array String -> Maybe (HH.HTML slots action)
arrangedOrDetail = arrangedDetail "or"

arrangedAndDetail :: forall slots action.
    String -> String -> Array String -> Maybe (HH.HTML slots action)
arrangedAndDetail = arrangedDetail "and"

onlineDetail :: forall slots actions.
    String -> Maybe { from :: String, to :: String } -> Maybe (HH.HTML slots actions)
onlineDetail _ Nothing = Nothing
onlineDetail frame (Just { from, to }) = Just $
    detail "fas fa-clock"
    [ HH.span [ HS.class_ "detail-labelless" ] [ HH.text $ "Online on " ]
    , HH.span [ HS.class_ "detail-emphasize" ] [ HH.text frame ]
    , HH.text " from "
    , HH.span [ HS.class_ "detail-emphasize" ] [ HH.text from ]
    , HH.text " to "
    , HH.span [ HS.class_ "detail-emphasize" ] [ HH.text to ]
    ]

weekdaysOnlineDetail :: forall slots actions.
    Maybe { from :: String, to :: String } -> Maybe (HH.HTML slots actions)
weekdaysOnlineDetail fromTo = onlineDetail "weekdays" fromTo

weekendsOnlineDetail :: forall slots actions.
    Maybe { from :: String, to :: String } -> Maybe (HH.HTML slots actions)
weekendsOnlineDetail fromTo = onlineDetail "weekends" fromTo

textDetail :: forall slots action. Array String -> Array (HH.HTML slots action)
textDetail paragraphs =
    paragraphs <#> \paragraph -> HH.p [ HS.class_ "detail-paragraph" ] [ HH.text paragraph ]

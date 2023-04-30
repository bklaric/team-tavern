module TeamTavern.Client.Components.Detail where

import Prelude

import Async (Async)
import Client.Components.Copyable (copyable)
import Data.Array as Array
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import TeamTavern.Client.Components.Anchor (textAnchor)
import TeamTavern.Client.Shared.Slot (Slot__String)
import TeamTavern.Client.Snippets.Class as HS
import Type.Proxy (Proxy(..))

detailColumnsContainer :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
detailColumnsContainer = HH.div [ HS.class_ "detail-columns-container" ]

detailColumns :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
detailColumns = HH.div [ HS.class_ "detail-columns" ]

detailColumn :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
detailColumn = HH.div [ HS.class_ "detail-column" ]

detailColumnHeading3 :: ∀ slots action. String -> HH.HTML slots action
detailColumnHeading3 heading = HH.h3 [ HS.class_ "detail-column-heading" ] [ HH.text heading ]

detailColumnHeading4 :: ∀ slots action. String -> HH.HTML slots action
detailColumnHeading4 heading = HH.h4 [ HS.class_ "detail-column-heading" ] [ HH.text heading ]

detail' :: ∀ slots action.
    HH.HTML slots action -> Array (HH.HTML slots action) -> HH.HTML slots action
detail' icon children = HH.p [ HS.class_ "detail" ] $ [ icon ] <> children

detail :: ∀ slots action. String -> Array (HH.HTML slots action) -> HH.HTML slots action
detail icon children = detail' (HH.i [ HS.class_ $ icon <> " detail-icon" ] []) children

fieldDetail' :: ∀ slots action.
    HH.HTML slots action -> String -> Array (HH.HTML slots action) -> HH.HTML slots action
fieldDetail' icon label children =
    detail' icon $
    [ HH.span [ HS.class_ "detail-label" ] [ HH.text $ label <> ": " ] ]
    <>
    children

fieldDetail :: ∀ slots action.
    String -> String -> Array (HH.HTML slots action) -> HH.HTML slots action
fieldDetail icon label children =
    detail icon $
    [ HH.span [ HS.class_ "detail-label" ] [ HH.text $ label <> ": " ] ]
    <>
    children

urlDetail :: ∀ slots action. String -> String -> String -> HH.HTML slots action
urlDetail icon text href = detail icon [ textAnchor "detail-url" href text ]

urlDetailMaybe :: ∀ slots action. String -> String -> Maybe String -> Maybe (HH.HTML slots action)
urlDetailMaybe _ _ Nothing = Nothing
urlDetailMaybe icon text (Just href) = Just $ urlDetail icon text href

discordTagDetail
    :: ∀ action left slots
    .  String
    -> Maybe String
    -> Maybe (HH.ComponentHTML action ( discordTag :: Slot__String | slots) (Async left))
discordTagDetail _ Nothing = Nothing
discordTagDetail nickname (Just discordTag) = Just $
    fieldDetail "fab fa-discord" "Discord tag"
    [ copyable (Proxy :: _ "discordTag") ("discordTag-" <> nickname) discordTag ]

arrangeItems :: ∀ slots action. String -> Array String -> Array (HH.HTML slots action)
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

arrangedDetail :: ∀ slots action.
    String -> String -> String -> Array String -> Maybe (HH.HTML slots action)
arrangedDetail _ _ _ items | Array.null items = Nothing
arrangedDetail binder icon prefix items = Just $
    detail icon $
    [ HH.span
        [ HS.class_ "detail-labelless" ]
        [ HH.text $ prefix <> " " ]
    ]
    <> arrangeItems binder items

arrangedOrDetail :: ∀ slots action.
    String -> String -> Array String -> Maybe (HH.HTML slots action)
arrangedOrDetail = arrangedDetail "or"

arrangedAndDetail :: ∀ slots action.
    String -> String -> Array String -> Maybe (HH.HTML slots action)
arrangedAndDetail = arrangedDetail "and"

onlineDetail :: ∀ slots actions.
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

weekdaysOnlineDetail :: ∀ slots actions.
    Maybe { from :: String, to :: String } -> Maybe (HH.HTML slots actions)
weekdaysOnlineDetail fromTo = onlineDetail "weekdays" fromTo

weekendsOnlineDetail :: ∀ slots actions.
    Maybe { from :: String, to :: String } -> Maybe (HH.HTML slots actions)
weekendsOnlineDetail fromTo = onlineDetail "weekends" fromTo

textDetail :: ∀ slots action. Array String -> Array (HH.HTML slots action)
textDetail paragraphs =
    paragraphs <#> \paragraph -> HH.p [ HS.class_ "detail-paragraph" ] [ HH.text paragraph ]

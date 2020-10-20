module TeamTavern.Client.Components.Detail where

import Prelude

import Data.Array as Array
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Class as HS

detailColumns :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
detailColumns = HH.div [ HS.class_ "detail-columns" ]

detailColumn :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
detailColumn = HH.div [ HS.class_ "detail-column" ]

detailColumnHeading :: forall slots action. String -> HH.HTML slots action
detailColumnHeading heading = HH.h4 [ HS.class_ "detail-column-heading" ] [ HH.text heading ]

detail :: forall slots action. String -> Array (HH.HTML slots action) -> HH.HTML slots action
detail icon children =
    HH.p
    [ HS.class_ "detail" ] $
    [ HH.i [ HS.class_ $ icon <> " detail-icon" ] [] ]
    <> children

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
            , itemsSoFar = [ HH.span [ HS.class_ "detail-emphasized" ] [ HH.text item ] ]
            }
        else if not state.secondItem
        then state
            { secondItem = true
            , itemsSoFar =
                [ HH.span [ HS.class_ "detail-emphasized" ] [ HH.text item ]
                , HH.text $ " " <> binder <> " "
                ]
                <> state.itemsSoFar
            }
        else state
            { itemsSoFar =
                [ HH.span [ HS.class_ "detail-emphasized" ] [ HH.text item ]
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
    , HH.span [ HS.class_ "detail-emphasized" ] [ HH.text frame ]
    , HH.text " from "
    , HH.span [ HS.class_ "detail-emphasized" ] [ HH.text from ]
    , HH.text " to "
    , HH.span [ HS.class_ "detail-emphasized" ] [ HH.text to ]
    ]

weekdaysOnlineDetail :: forall slots actions.
    Maybe { from :: String, to :: String } -> Maybe (HH.HTML slots actions)
weekdaysOnlineDetail fromTo = onlineDetail "weekdays" fromTo

weekendsOnlineDetail :: forall slots actions.
    Maybe { from :: String, to :: String } -> Maybe (HH.HTML slots actions)
weekendsOnlineDetail fromTo = onlineDetail "weekends" fromTo

textDetail :: forall slots action. Array String -> Maybe (Array (HH.HTML slots action))
textDetail paragraphs | Array.null paragraphs = Nothing
textDetail paragraphs = Just $
    paragraphs <#> \paragraph -> HH.p [ HS.class_ "detail-paragraph" ] [ HH.text paragraph ]

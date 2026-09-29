module TeamTavern.Client.Components.DataList
    ( DataRow
    , dataList
    , personRow
    , personRows
    , row
    ) where

import Prelude

import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Class as HS

-- A row with an id can be linked to and takes the focus when it is, as
-- /account#emails does.
type DataRow w i =
    { id :: Maybe String
    , label :: String
    , value :: Array (HH.HTML w i)
    , action :: Maybe (HH.HTML w i)
    }

row :: ∀ w i. String -> Array (HH.HTML w i) -> DataRow w i
row label value = { id: Nothing, label, value, action: Nothing }

-- Labels, what the account holds and the way to change it, one row each.
dataList :: ∀ w i. Array (DataRow w i) -> HH.HTML w i
dataList rows =
    HH.dl [ HS.class_ "data-list" ] $ rows <#> \{ id, label, value, action } ->
        HH.div
        ( [ HS.class_ "data-row" ]
            <> case id of
                Just id' -> [ HP.id id', HP.tabIndex (-1) ]
                Nothing -> []
        )
        $ catMaybes
            [ Just $ HH.dt_ [ HH.text label ]
            , Just $ HH.dd [ HS.class_ "data-value" ] value
            , action <#> \action' -> HH.dd [ HS.class_ "data-action" ] [ action' ]
            ]

-- Players named with one action each, such as the blocked list, keyed by
-- name, since the action takes a player off the list.
personRows :: ∀ w i. Array (Tuple String (HH.HTML w i)) -> HH.HTML w i
personRows = HK.ul [ HS.class_ "person-rows" ]

personRow :: ∀ w i. String -> HH.HTML w i -> Tuple String (HH.HTML w i)
personRow name action = Tuple name $
    HH.li [ HS.class_ "person-row" ] [ HH.span [ HS.class_ "person-name" ] [ HH.text name ], action ]

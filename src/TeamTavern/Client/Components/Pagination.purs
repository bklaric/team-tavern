module TeamTavern.Client.Components.Pagination (pagination) where

import Prelude

import Data.Int (ceil, toNumber)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Profile.Shared (pageSize)

totalPages :: Int -> Int
totalPages count = ceil (toNumber count / toNumber pageSize)

pagination :: ∀ slots action. Int -> Int -> (Int -> action) -> HH.HTML slots action
pagination page profileCount changePage =
    HH.div [ HS.class_ "pagination" ]
    [ HH.div [ HP.class_$ HH.ClassName "pagination-left-buttons" ]
        [ HH.button
            [ HS.class_ "pagination-first-button"
            , HP.disabled $ page == 1
            , HE.onClick $ const $ changePage 1
            ]
            [ HH.text "First" ]
        , HH.button
            [ HS.class_ "pagination-previous-button"
            , HP.disabled $ page == 1
            , HE.onClick $ const $ changePage $ page - 1
            ]
            [ HH.text "Previous" ]
        ]
    , HH.div [ HS.class_ "pagination-page" ]
        [ HH.text $ show page <> "/" <> show (totalPages profileCount) ]
    , HH.div [ HP.class_$ HH.ClassName "pagination-right-buttons" ]
        [ HH.button
            [ HS.class_ "pagination-next-button"
            , HP.disabled $ page == (totalPages profileCount)
            , HE.onClick $ const $ changePage $ page + 1
            ]
            [ HH.text "Next" ]
        , HH.button
            [ HS.class_ "pagination-last-button"
            , HP.disabled $ page == (totalPages profileCount)
            , HE.onClick $ const $ changePage $ totalPages profileCount
            ]
            [ HH.text "Last" ]
        ]
    ]

module TeamTavern.Client.Pages.Profiles.Ads (profileSectionsWithAds, descriptionAd, filtersAd) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Server.Profile.ViewTeamProfilesByGame.LoadProfiles (pageSize)

sectionAd :: forall slots action. HH.HTML slots action
sectionAd =
    HH.div [ HS.class_ "card-section", HP.prop (H.PropName "style") "min-width: 100%; padding: 0;", HP.attr (H.AttrName "align") "center" ]
    [ HH.script
        [ HP.prop (H.PropName "async") true
        , HP.src "https://pagead2.googlesyndication.com/pagead/js/adsbygoogle.js"
        ]
        []
    , HH.ins
        [ HS.class_ "adsbygoogle"
        , HP.prop (H.PropName "style") "display:block;"
        -- , HP.attr (H.AttrName "data-ad-format") "fluid"
        -- , HP.attr (H.AttrName "data-ad-layout-key") "-fz+40+cb-ag-hq"
        -- , HP.attr (H.AttrName "data-ad-client") "ca-pub-4943861576032109"
        -- , HP.attr (H.AttrName "data-ad-slot") "2422006799"
        , HP.attr (H.AttrName "data-ad-client") "ca-pub-4943861576032109"
        , HP.attr (H.AttrName "data-ad-slot") "5207311601"
        , HP.attr (H.AttrName "data-ad-format") "auto"
        , HP.attr (H.AttrName "data-full-width-responsive") "true"
        ]
        []
    , HH.script_ [ HH.text "(adsbygoogle = window.adsbygoogle || []).push({});" ]
    ]

profileSectionsWithAds :: forall slots action.
    Array (HH.HTML slots action) -> Array (HH.HTML slots action)
profileSectionsWithAds profileSections = let
    firstIndex = pageSize / 3
    secondIndex = pageSize / 3 * 2 + 1
    thirdIndex = pageSize / 4 * 4 + 2
    in
    case Array.insertAt firstIndex sectionAd profileSections of
    Just profileSections' ->
        case Array.insertAt secondIndex sectionAd profileSections' of
        Just profileSections'' ->
            case Array.insertAt thirdIndex sectionAd profileSections'' of
            Just profileSections''' -> profileSections'''
            Nothing -> profileSections''
        Nothing -> profileSections'
    Nothing -> Array.snoc profileSections sectionAd

descriptionAd :: forall slots action. Int -> HH.HTML slots action
descriptionAd width =
    HH.div
    [ HP.prop (H.PropName "style") "width: 100%; padding: 0 21px; margin-bottom: 21px; box-sizing: border-box;"
    , HP.attr (H.AttrName "align") "center"
    ]
    [ HH.script
        [ HP.prop (H.PropName "async") true
        , HP.src "https://pagead2.googlesyndication.com/pagead/js/adsbygoogle.js"
        ]
        []
    , HH.ins
        [ HS.class_ "adsbygoogle"
        , HP.prop (H.PropName "style") $ "display: block;"
        , HP.attr (H.AttrName "data-ad-client") "ca-pub-4943861576032109"
        , HP.attr (H.AttrName "data-ad-slot") "5207311601"
        , HP.attr (H.AttrName "data-ad-format") "auto"
        , HP.attr (H.AttrName "data-full-width-responsive") "true"
        ]
        []
    , HH.script_ [ HH.text "(adsbygoogle = window.adsbygoogle || []).push({});" ]
    ]

filtersAd :: forall slots action. HH.HTML slots action
filtersAd =
    HH.div
    [ HP.prop (H.PropName "style") "position: sticky; top: 64px;"
    , HP.attr (H.AttrName "align") "center"
    ]
    [ HH.script
        [ HP.prop (H.PropName "async") true
        , HP.src "https://pagead2.googlesyndication.com/pagead/js/adsbygoogle.js"
        ]
        []
    , HH.ins
        [ HS.class_ "adsbygoogle"
        , HP.prop (H.PropName "style") $ "display: block;"
        , HP.attr (H.AttrName "data-ad-client") "ca-pub-4943861576032109"
        , HP.attr (H.AttrName "data-ad-slot") "4168496441"
        , HP.attr (H.AttrName "data-ad-format") "vertical"
        , HP.attr (H.AttrName "data-full-width-responsive") "true"
        ]
        []
    , HH.script_ [ HH.text "(adsbygoogle = window.adsbygoogle || []).push({});" ]
    ]

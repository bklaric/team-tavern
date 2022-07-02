module TeamTavern.Client.Snippets.Brands where

import Prelude

import Halogen (AttrName(..), ClassName(..))
import Halogen.HTML (HTML, attr)
import Halogen.Svg.Attributes (viewBox)
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Elements (path, svg)
import TeamTavern.Routes.Shared.Platform (Platform(..))

platformSvg :: forall slots action. String -> Platform -> HTML slots action
platformSvg class' Steam = steamSvg class'
platformSvg class' Riot = riotSvg class'
platformSvg class' BattleNet = battleNetSvg class'
platformSvg class' Origin = originSvg class'
platformSvg class' PlayStation = playStationSvg class'
platformSvg class' Xbox = xboxSvg class'
platformSvg class' Switch = switchSvg class'

-- Steam

steamPath :: String
steamPath = "M11.979 0C5.678 0 .511 4.86.022 11.037l6.432 2.658c.545-.371 1.203-.59 1.912-.59.063 0 .125.004.188.006l2.861-4.142V8.91c0-2.495 2.028-4.524 4.524-4.524 2.494 0 4.524 2.031 4.524 4.527s-2.03 4.525-4.524 4.525h-.105l-4.076 2.911c0 .052.004.105.004.159 0 1.875-1.515 3.396-3.39 3.396-1.635 0-3.016-1.173-3.331-2.727L.436 15.27C1.862 20.307 6.486 24 11.979 24c6.627 0 11.999-5.373 11.999-12S18.605 0 11.979 0zM7.54 18.21l-1.473-.61c.262.543.714.999 1.314 1.25 1.297.539 2.793-.076 3.332-1.375.263-.63.264-1.319.005-1.949s-.75-1.121-1.377-1.383c-.624-.26-1.29-.249-1.878-.03l1.523.63c.956.4 1.409 1.5 1.009 2.455-.397.957-1.497 1.41-2.454 1.012H7.54zm11.415-9.303c0-1.662-1.353-3.015-3.015-3.015-1.665 0-3.015 1.353-3.015 3.015 0 1.665 1.35 3.015 3.015 3.015 1.663 0 3.015-1.35 3.015-3.015zm-5.273-.005c0-1.252 1.013-2.266 2.265-2.266 1.249 0 2.266 1.014 2.266 2.266 0 1.251-1.017 2.265-2.266 2.265-1.253 0-2.265-1.014-2.265-2.265z"

steamSvg :: forall slots actions. String -> HTML slots actions
steamSvg class' = svg [ HSA.class_ $ ClassName class', viewBox 0.0 0.0 24.0 24.0 ] [ path [ attr (AttrName "d") steamPath ] ]

inputSteamSvg :: forall slots actions. HTML slots actions
inputSteamSvg = steamSvg "input-label-icon"

detailSteamSvg :: forall slots actions. HTML slots actions
detailSteamSvg = steamSvg "detail-icon"

-- Riot

riotPath :: String
riotPath = "M12.534 21.77l-1.09-2.81 10.52.54-.451 4.5zM15.06 0L.307 6.969 2.59 17.471H5.6l-.52-7.512.461-.144 1.81 7.656h3.126l-.116-9.15.462-.144 1.582 9.294h3.31l.78-11.053.462-.144.82 11.197h4.376l1.54-15.37Z"

riotSvg :: forall slots actions. String -> HTML slots actions
riotSvg class' = svg [ HSA.class_ $ ClassName class', viewBox 0.0 0.0 24.0 24.0 ] [ path [ attr (AttrName "d") riotPath ] ]

inputRiotSvg :: forall slots actions. HTML slots actions
inputRiotSvg = riotSvg "input-label-icon"

detailRiotSvg :: forall slots actions. HTML slots actions
detailRiotSvg = riotSvg "detail-icon"

-- Battle.net

battleNetPath :: String
battleNetPath = "M10.457 0c-.516.02-.859.314-1.053.523-.807.87-1.136 2.298-1.168 3.952-.304-.522-.72-1.061-1.199-1.198a.905.905 0 00-.172-.03c-.958-.138-1.768 1.393-1.66 3.812-1.8.064-3.33.268-4.363.525-.182.045-.312.1-.42.154-.075.035-.128.07-.18.104-.162.106-.234.199-.234.199.47-.177 2.358-.495 5.234-.387l-.004-.045h.006c.126 1.29.502 2.789 1.235 4.409v.003l-.024-.044c-.456.721-1.792 2.923-2.217 4.58-.277 1.081-.202 1.772.014 2.218.262.59.764.776 1.08.848 1.173.268 2.6-.176 4.068-.998-.316.537-.603 1.204-.476 1.709a.881.881 0 00.058.162c.363.897 2.091.832 4.131-.47.948 1.51 1.882 2.72 2.616 3.48.13.136.243.223.345.289.277.195.467.205.467.205-.387-.316-1.598-1.78-2.934-4.303l-.035.028c0-.002-.003-.005-.004-.006 1.064-.76 2.186-1.847 3.23-3.31h.003l-.028.038-.002.004c.896.034 3.41.08 5.03-.373 1.07-.299 1.63-.706 1.91-1.115.383-.523.293-1.054.197-1.365-.354-1.15-1.448-2.16-2.892-3.022.622.005 1.342-.08 1.714-.441a.884.884 0 00.116-.139c.587-.764-.335-2.227-2.479-3.34.834-1.576 1.417-2.989 1.71-4.004.05-.179.067-.319.073-.44.032-.339-.054-.509-.054-.509-.08.493-.743 2.271-2.26 4.69l.041.02-.002.003c-1.19-.54-2.693-.968-4.482-1.14l-.002-.003.05.004c-.418-.793-1.633-2.992-2.834-4.168-.792-.775-1.426-1.058-1.92-1.097a1.532 1.532 0 00-.23-.012zm1.172 2.643c.461.008.936.364 1.328.738.491.47 1.111 1.374 1.412 1.83-.083-.003-.161-.014-.246-.016-1.863-.047-3.216.366-4.195.98.06-1.543.419-2.8 1.238-3.374a.847.847 0 01.463-.158zM7.514 4.71c.03 0 .06.007.09.012.256.07.471.338.642.642.023.563.075 1.144.15 1.733a34.71 34.71 0 00-1.988-.06c.041-1.377.428-2.31 1.106-2.327zm5.478 1.21c.244-.007.494-.003.752.013 2.092.125 4.045.717 5.45 1.443-.33.486-.696.993-1.09 1.514-.601-1.09-1.467-1.74-1.868-1.91-.349-.15-.422-.14-.422-.14s.033-.01.57.413c.579.455 1.046 1.106 1.376 1.805a33.723 33.723 0 00-5.405-1.489 30.386 30.386 0 00-1.847-.283c-.002.011-.002.033-.004.045l-.025-.004c-.016.111-.036.277-.05.46-.014.2-.02.358-.023.452.157.03.316.058.475.09 2.275.45 5.224 1.381 7.363 2.596.034 1.103-.325 2.417-1.19 3.726-1.154 1.75-2.644 3.147-3.976 4a35.941 35.941 0 01-.767-1.705c1.266.037 2.282-.395 2.634-.66.3-.224.33-.294.33-.297-.001.004-.03.044-.64.287-.696.278-1.51.356-2.293.285a33.748 33.748 0 003.988-3.931c.408-.478.797-.967 1.168-1.46l-.035-.025.016-.019a7.198 7.198 0 00-.754-.518l-.315.366c-1.522 1.74-3.794 3.819-5.91 5.066-.964-.525-1.913-1.49-2.61-2.88-.936-1.874-1.4-3.863-1.474-5.442.573.042 1.183.106 1.816.185-.644 1.066-.775 2.144-.722 2.576.045.372.09.43.092.432-.002-.002-.022-.046.072-.697.105-.728.432-1.46.873-2.094a33.707 33.707 0 001.414 5.422c.21.593.437 1.173.678 1.74l.039-.015.011.023c.105-.042.258-.107.422-.187.181-.088.32-.162.403-.208-.054-.15-.108-.303-.16-.457-.748-2.194-1.414-5.212-1.432-7.671.784-.486 1.833-.808 3.07-.846zm6.793 1.788c1.172.724 1.788 1.526 1.465 2.121-.182.264-.605.323-1.025.307a20.285 20.285 0 00-1.504-.7c.383-.582.738-1.162 1.064-1.728zm-1.033 3.518c1.307.823 2.215 1.76 2.303 2.757a.85.85 0 01-.096.485.987.987 0 01-.11.154c-.273.303-.743.49-1.19.621-.653.19-1.746.277-2.292.31.045-.07.09-.132.135-.204.973-1.59 1.293-2.968 1.25-4.123zM6.93 12.936c.046.088.084.173.133.261.883 1.626 1.907 2.59 2.921 3.133-1.374.727-2.647 1.051-3.558.627a.852.852 0 01-.453-.5c-.123-.388-.052-.888.058-1.34.166-.68.662-1.71.899-2.181zm4.6 4.273c.313.625.637 1.223.964 1.789-1.212.652-2.212.785-2.566.207-.017-.027-.026-.059-.037-.088-.075-.28.08-.633.283-.955.453-.29.907-.611 1.355-.953Z"

battleNetSvg :: forall slots actions. String -> HTML slots actions
battleNetSvg class' = svg [ HSA.class_ $ ClassName class', viewBox 0.0 0.0 24.0 24.0 ] [ path [ attr (AttrName "d") battleNetPath ] ]

inputBattleNetSvg :: forall slots actions. HTML slots actions
inputBattleNetSvg = battleNetSvg "input-label-icon"

detailBattleNetSvg :: forall slots actions. HTML slots actions
detailBattleNetSvg = battleNetSvg "detail-icon"

-- Origin

originPath :: String
originPath = "M12.588 3.11c1.189.071 2.352.384 3.417.919 1.031.514 1.95 1.225 2.706 2.094.751.865 1.322 1.853 1.715 2.963.391 1.109.548 2.278.464 3.502-.033.636-.135 1.252-.306 1.848-.167.588-.393 1.159-.674 1.703-.439.849-.929 1.652-1.47 2.412-.538.759-1.125 1.465-1.762 2.118-.638.653-1.313 1.254-2.032 1.802-.719.544-1.471 1.038-2.254 1.479l-.037.026c-.033.018-.071.026-.109.023-.063-.015-.118-.048-.159-.097-.041-.05-.063-.111-.062-.173 0-.029.004-.059.012-.085.008-.023.021-.044.037-.062.277-.393.506-.806.686-1.235.181-.434.303-.885.368-1.359 0-.032-.015-.064-.038-.085-.021-.025-.053-.038-.085-.038-.264.032-.528.053-.795.062-.266.009-.532-.003-.796-.037-1.189-.071-2.353-.385-3.418-.918-1.031-.515-1.949-1.226-2.705-2.095-.754-.87-1.336-1.875-1.715-2.963-.394-1.123-.552-2.314-.465-3.502.033-.636.135-1.252.306-1.848.171-.598.396-1.155.675-1.68.439-.864.931-1.676 1.469-2.436.539-.757 1.125-1.464 1.761-2.118.639-.652 1.314-1.252 2.033-1.8.72-.546 1.47-1.039 2.253-1.479l.038-.025c.033-.02.07-.027.109-.025.065.016.119.051.158.098.043.051.062.106.062.174.001.027-.003.057-.012.084-.007.023-.02.043-.036.061-.273.386-.505.801-.687 1.237-.181.433-.3.885-.366 1.358 0 .033.012.063.036.086.022.024.054.037.085.037.262-.033.527-.053.795-.061.272-.009.536.003.798.035zm-.807 12.367c.922.079 1.838-.231 2.521-.855.72-.639 1.109-1.438 1.176-2.4.078-.928-.232-1.846-.856-2.535-.601-.708-1.472-1.131-2.4-1.162-.927-.078-1.845.232-2.534.855-.709.602-1.132 1.473-1.164 2.4-.078.926.228 1.842.846 2.535.628.725 1.432 1.115 2.411 1.162z"

originSvg :: forall slots actions. String -> HTML slots actions
originSvg class' = svg [ HSA.class_ $ ClassName class', viewBox 0.0 0.0 24.0 24.0 ] [ path [ attr (AttrName "d") originPath ] ]

inputOriginSvg :: forall slots actions. HTML slots actions
inputOriginSvg = originSvg "input-label-icon"

detailOriginSvg :: forall slots actions. HTML slots actions
detailOriginSvg = originSvg "detail-icon"

-- PlayStation

playStationPath :: String
playStationPath = "M8.984 2.596v17.547l3.915 1.261V6.688c0-.69.304-1.151.794-.991.636.18.76.814.76 1.505v5.875c2.441 1.193 4.362-.002 4.362-3.152 0-3.237-1.126-4.675-4.438-5.827-1.307-.448-3.728-1.186-5.39-1.502zm4.656 16.241l6.296-2.275c.715-.258.826-.625.246-.818-.586-.192-1.637-.139-2.357.123l-4.205 1.5V14.98l.24-.085s1.201-.42 2.913-.615c1.696-.18 3.785.03 5.437.661 1.848.601 2.04 1.472 1.576 2.072-.465.6-1.622 1.036-1.622 1.036l-8.544 3.107V18.86zM1.807 18.6c-1.9-.545-2.214-1.668-1.352-2.32.801-.586 2.16-1.052 2.16-1.052l5.615-2.013v2.313L4.205 17c-.705.271-.825.632-.239.826.586.195 1.637.15 2.343-.12L8.247 17v2.074c-.12.03-.256.044-.39.073-1.939.331-3.996.196-6.038-.479z"

playStationSvg :: forall slots actions. String -> HTML slots actions
playStationSvg class' = svg [ HSA.class_ $ ClassName class', viewBox 0.0 0.0 24.0 24.0 ] [ path [ attr (AttrName "d") playStationPath ] ]

inputPlayStationSvg :: forall slots actions. HTML slots actions
inputPlayStationSvg = playStationSvg "input-label-icon"

detailPlayStationSvg :: forall slots actions. HTML slots actions
detailPlayStationSvg = playStationSvg "detail-icon"

-- Xbox

xboxPath :: String
xboxPath = "M4.102 21.033C6.211 22.881 8.977 24 12 24c3.026 0 5.789-1.119 7.902-2.967 1.877-1.912-4.316-8.709-7.902-11.417-3.582 2.708-9.779 9.505-7.898 11.417zm11.16-14.406c2.5 2.961 7.484 10.313 6.076 12.912C23.002 17.48 24 14.861 24 12.004c0-3.34-1.365-6.362-3.57-8.536 0 0-.027-.022-.082-.042-.063-.022-.152-.045-.281-.045-.592 0-1.985.434-4.805 3.246zM3.654 3.426c-.057.02-.082.041-.086.042C1.365 5.642 0 8.664 0 12.004c0 2.854.998 5.473 2.661 7.533-1.401-2.605 3.579-9.951 6.08-12.91-2.82-2.813-4.216-3.245-4.806-3.245-.131 0-.223.021-.281.046v-.002zM12 3.551S9.055 1.828 6.755 1.746c-.903-.033-1.454.295-1.521.339C7.379.646 9.659 0 11.984 0H12c2.334 0 4.605.646 6.766 2.085-.068-.046-.615-.372-1.52-.339C14.946 1.828 12 3.545 12 3.545v.006z"

xboxSvg :: forall slots actions. String -> HTML slots actions
xboxSvg class' = svg [ HSA.class_ $ ClassName class', viewBox 0.0 0.0 24.0 24.0 ] [ path [ attr (AttrName "d") xboxPath ] ]

inputXboxSvg :: forall slots actions. HTML slots actions
inputXboxSvg = xboxSvg "input-label-icon"

detailXboxSvg :: forall slots actions. HTML slots actions
detailXboxSvg = xboxSvg "detail-icon"

-- Switch

switchPath :: String
switchPath = "M14.176 24h3.674c3.376 0 6.15-2.774 6.15-6.15V6.15C24 2.775 21.226 0 17.85 0H14.1c-.074 0-.15.074-.15.15v23.7c-.001.076.075.15.226.15zm4.574-13.199c1.351 0 2.399 1.125 2.399 2.398 0 1.352-1.125 2.4-2.399 2.4-1.35 0-2.4-1.049-2.4-2.4-.075-1.349 1.05-2.398 2.4-2.398zM11.4 0H6.15C2.775 0 0 2.775 0 6.15v11.7C0 21.226 2.775 24 6.15 24h5.25c.074 0 .15-.074.15-.149V.15c.001-.076-.075-.15-.15-.15zM9.676 22.051H6.15c-2.326 0-4.201-1.875-4.201-4.201V6.15c0-2.326 1.875-4.201 4.201-4.201H9.6l.076 20.102zM3.75 7.199c0 1.275.975 2.25 2.25 2.25s2.25-.975 2.25-2.25c0-1.273-.975-2.25-2.25-2.25s-2.25.977-2.25 2.25z"

switchSvg :: forall slots actions. String -> HTML slots actions
switchSvg class' = svg [ HSA.class_ $ ClassName class', viewBox 0.0 0.0 24.0 24.0 ] [ path [ attr (AttrName "d") switchPath ] ]

inputSwitchSvg :: forall slots actions. HTML slots actions
inputSwitchSvg = switchSvg "input-label-icon"

detailSwitchSvg :: forall slots actions. HTML slots actions
detailSwitchSvg = switchSvg "detail-icon"

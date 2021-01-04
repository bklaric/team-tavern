module TeamTavern.Client.Snippets.Brands where

import Halogen (AttrName(..))
import Halogen.HTML (HTML)
import Halogen.Svg.Attributes (attr, class_, viewBox)
import Halogen.Svg.Elements (path, svg)

steamPath :: String
steamPath = "M11.979 0C5.678 0 .511 4.86.022 11.037l6.432 2.658c.545-.371 1.203-.59 1.912-.59.063 0 .125.004.188.006l2.861-4.142V8.91c0-2.495 2.028-4.524 4.524-4.524 2.494 0 4.524 2.031 4.524 4.527s-2.03 4.525-4.524 4.525h-.105l-4.076 2.911c0 .052.004.105.004.159 0 1.875-1.515 3.396-3.39 3.396-1.635 0-3.016-1.173-3.331-2.727L.436 15.27C1.862 20.307 6.486 24 11.979 24c6.627 0 11.999-5.373 11.999-12S18.605 0 11.979 0zM7.54 18.21l-1.473-.61c.262.543.714.999 1.314 1.25 1.297.539 2.793-.076 3.332-1.375.263-.63.264-1.319.005-1.949s-.75-1.121-1.377-1.383c-.624-.26-1.29-.249-1.878-.03l1.523.63c.956.4 1.409 1.5 1.009 2.455-.397.957-1.497 1.41-2.454 1.012H7.54zm11.415-9.303c0-1.662-1.353-3.015-3.015-3.015-1.665 0-3.015 1.353-3.015 3.015 0 1.665 1.35 3.015 3.015 3.015 1.663 0 3.015-1.35 3.015-3.015zm-5.273-.005c0-1.252 1.013-2.266 2.265-2.266 1.249 0 2.266 1.014 2.266 2.266 0 1.251-1.017 2.265-2.266 2.265-1.253 0-2.265-1.014-2.265-2.265z"

steamSvg :: forall slots actions. String -> HTML slots actions
steamSvg class' = svg [ class_ class', viewBox 0.0 0.0 24.0 24.0 ] [ path [ attr (AttrName "d") steamPath ] ]

inputSteamSvg :: forall slots actions. HTML slots actions
inputSteamSvg = steamSvg "input-label-icon"

detailSteamSvg :: forall slots actions. HTML slots actions
detailSteamSvg = steamSvg "detail-icon"

riotPath :: String
riotPath = "M12.534 21.77l-1.09-2.81 10.52.54-.451 4.5zM15.06 0L.307 6.969 2.59 17.471H5.6l-.52-7.512.461-.144 1.81 7.656h3.126l-.116-9.15.462-.144 1.582 9.294h3.31l.78-11.053.462-.144.82 11.197h4.376l1.54-15.37Z"

riotSvg :: forall slots actions. String -> HTML slots actions
riotSvg class' = svg [ class_ class', viewBox 0.0 0.0 24.0 24.0 ] [ path [ attr (AttrName "d") riotPath ] ]

inputRiotSvg :: forall slots actions. HTML slots actions
inputRiotSvg = riotSvg "input-label-icon"

detailRiotSvg :: forall slots actions. HTML slots actions
detailRiotSvg = riotSvg "detail-icon"

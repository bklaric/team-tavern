module TeamTavern.Client.Pages.Player.CreateProfileButton where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Halogen (lift)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Popover (popover, popoverButtonCaret, popoverItem, togglePopover, usePopover)
import TeamTavern.Client.Pages.Player.CreateProfile (createProfile)
import TeamTavern.Client.Script.Request (get)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Server.Game.View.SendResponse as View
import TeamTavern.Server.Game.ViewAll.SendResponse as ViewAll

type Input =
    { nickname :: String
    , profileGameHandles :: Array String
    }

createProfileButton :: forall left output query. H.Component HH.HTML query Input output (Async left)
createProfileButton = Hooks.component $ \_ { nickname, profileGameHandles } -> Hooks.do
    (Tuple shown shownId) <- usePopover

    (Tuple (games :: ViewAll.OkContent) gamesId) <- Hooks.useState []

    Hooks.useLifecycleEffect do
        games' <- lift $ get "/api/games"
        let games'' = games' # maybe [] \games_ ->
                games_ # Array.filter (\{ handle } -> not $ Array.elem handle profileGameHandles)
        Hooks.put gamesId games''
        pure Nothing

    (Tuple modalShown modalShownId) <- Hooks.useState Nothing

    Hooks.pure $
        if Array.null games
        then HH.div_ []
        else
        popover
        shown
        ([ HH.button
            [ HS.class_ "primary-button"
            , HE.onClick (Just <<< togglePopover shownId)
            ]
            [ HH.i [ HS.class_ "fas fa-user-plus button-icon" ] []
            , HH.text "Create player profile"
            , popoverButtonCaret shown
            ]
        ]
        <>
        case modalShown of
        Nothing -> []
        Just modalInput ->
            [ createProfile
                modalInput
                (const $ Just $ Hooks.put modalShownId Nothing)
            ]
        )
        (games <#> \game ->
            popoverItem
            (const do
                game' <- lift $ get $ "/api/games/by-handle/" <> game.handle
                case game' of
                    Nothing -> pure unit
                    Just (game'' :: View.OkContent) ->
                        Hooks.put modalShownId $ Just
                        { nickname
                        , handle: game''.handle
                        , title: game''.title
                        , fields: game''.fields
                        }
            )
            [ HH.img
                [ HS.class_ "game-card-logo"
                , HP.src $ "/images/" <> game.handle <> "-icon-black.png"
                ]
            , HH.text game.title
            ]
        )
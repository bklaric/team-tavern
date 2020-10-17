module TeamTavern.Client.Team.CreateProfileButton where

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
import TeamTavern.Client.Components.Popover (popover, popoverButtonCaret, togglePopover, usePopover)
import TeamTavern.Client.Pages.Team.CreateProfile (createProfile)
import TeamTavern.Client.Script.Request (get)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Server.Game.View.SendResponse as View
import TeamTavern.Server.Game.ViewAll.SendResponse as ViewAll

createProfileButton :: forall left output query.
    H.Component HH.HTML query { teamHandle :: String } output (Async left)
createProfileButton = Hooks.component $ \_ { teamHandle } -> Hooks.do
    (Tuple shown shownId) <- usePopover

    (Tuple (games :: ViewAll.OkContent) gamesId) <- Hooks.useState []

    Hooks.useLifecycleEffect do
        games' <- lift $ get "/api/games"
        Hooks.put gamesId (maybe [] identity games')
        pure Nothing

    (Tuple modalShown modalShownId) <- Hooks.useState Nothing

    Hooks.pure $
        popover
        shown
        ([ HH.button
            [ HS.class_ "primary-button"
            , HE.onClick (Just <<< togglePopover shownId)
            ]
            [ HH.i [ HS.class_ "fas fa-user-plus button-icon" ] []
            , HH.text "Create team profile"
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
            HH.div
            [ HS.class_ "popover-item"
            , HE.onClick $ const $ Just do
                game' <- lift $ get $ "/api/games/by-handle/" <> game.handle
                case game' of
                    Nothing -> pure unit
                    Just (game'' :: View.OkContent) ->
                        Hooks.put modalShownId $ Just
                        { teamHandle
                        , gameHandle: game''.handle
                        , title: game''.title
                        , fields:
                            game''.fields
                            <#> (\{ key, label, icon, options } ->
                                options <#> { key, label, icon, options: _ })
                            # Array.catMaybes
                        }
            ]
            [ HH.img
                [ HS.class_ "game-card-logo"
                , HP.src $ "/images/" <> game.handle <> "-icon-black.png"
                ]
            , HH.text game.title
            ]
        )

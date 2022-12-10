module TeamTavern.Client.Pages.Player.CreateProfileButton where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
import Data.Foldable (foldMap)
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
import TeamTavern.Routes.Game.ViewAllGames as ViewAllGames
import TeamTavern.Routes.Player.ViewPlayer as ViewPlayer

type Input = ViewPlayer.OkContent

type Slot = H.Slot (Const Void) Void Unit

createProfileButton :: ∀ left output query. H.Component query Input output (Async left)
createProfileButton = Hooks.component $ \_ player -> Hooks.do
    (Tuple shown shownId) <- usePopover

    (Tuple (games :: ViewAllGames.OkContent) gamesId) <- Hooks.useState []

    let profileGameHandles = player.profiles <#> _.handle

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
            , HE.onClick $ togglePopover shownId
            ]
            [ HH.i [ HS.class_ "fas fa-user-plus button-icon" ] []
            , HH.text "Create player profile"
            , popoverButtonCaret shown
            ]
        ]
        <>
        foldMap (\modalInput ->
            [ createProfile modalInput (const $ Hooks.put modalShownId Nothing) ])
            modalShown
        )
        (games <#> \game ->
            popoverItem
            (const do
                game' <- lift $ get $ "/api/games/" <> game.handle
                case game' of
                    Nothing -> pure unit
                    Just game'' -> Hooks.put modalShownId $ Just { player, game: game'' }
            )
            [ HH.img
                [ HS.class_ "game-card-logo"
                , HP.src $ "/images/" <> game.handle <> "/icon-black.png"
                ]
            , HH.text game.title
            ]
        )

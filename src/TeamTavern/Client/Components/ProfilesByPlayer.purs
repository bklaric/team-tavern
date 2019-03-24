module TeamTavern.Client.Components.ProfilesByPlayer
    (Slot, profilesByPlayer) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen (ClassName(..), defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.EditProfile (editProfile)
import TeamTavern.Client.EditProfile as EditProfile
import TeamTavern.Client.Script.Cookie (getPlayerNickname)
import TeamTavern.Profile.ViewByPlayer.SendResponse as ViewByPlayer
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data Action
    = Init String
    | ShowEditProfileModal EditProfile.Input MouseEvent
    | HandleEditProfileMessage (Modal.Message EditProfile.Message)

data State
    = Empty
    | Profiles ViewByPlayer.OkContent (Maybe String)

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( games :: Anchor.Slot Int
    , editProfile :: EditProfile.Slot Unit
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render Empty = HH.div_ []
render (Profiles profiles nickname') = HH.div_ $
    [ HH.h3_ [ HH.text "Profiles" ] ] <>
    (profiles # mapWithIndex \index { handle, title, summary } ->
        HH.div [ HP.class_ $ ClassName "profile-item" ] $ join
        [ pure $
            HH.h3_ [ navigationAnchorIndexed (SProxy :: SProxy "games") index
            { path: "/games/" <> handle, text: title } ]
        , case nickname' of
            Nothing -> []
            Just nickname -> pure $ HH.p_ [
                HH.a
                [ HP.href ""
                , HE.onClick $ Just <<< ShowEditProfileModal
                    { handle, title, nickname, summary }
                ]
                [ HH.text "Edit profile" ] ]
        , summary <#> \paragraph -> HH.p_ [ HH.text paragraph ]
        ])
    <> case nickname' of
        Nothing -> []
        Just _ -> [ editProfile $ Just <<< HandleEditProfileMessage ]

loadProfiles :: forall left. String -> Async left State
loadProfiles nickname = Async.unify do
    response <-  Fetch.fetch_ ("/api/profiles?nickname=" <> nickname)
        # lmap (const Empty)
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Empty)
        _ -> Async.left Empty
    playerNickname <- H.liftEffect getPlayerNickname
    pure $ Profiles content $ case playerNickname of
        Just playerNickname' | playerNickname' == nickname ->
            Just playerNickname'
        _ -> Nothing

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction (Init nickname) = do
    state <- H.lift $ loadProfiles nickname
    H.put state
    pure unit
handleAction (ShowEditProfileModal profile event) = do
    H.liftEffect $ preventDefault $ toEvent event
    Modal.showWith profile (SProxy :: SProxy "editProfile")
handleAction (HandleEditProfileMessage message) = do
    Modal.hide (SProxy :: SProxy "editProfile")
    case message of
        Modal.Inner (EditProfile.ProfileUpdated nickname) ->
            handleAction $ Init nickname
        _ -> pure unit

component :: forall output left query.
    String -> H.Component HH.HTML query String output (Async left)
component nickname = mkComponent
    { initialState: const Empty
    , render
    , eval: mkEval $ defaultEval
        { handleAction = handleAction
        , initialize = Just $ Init nickname
        , receive = Just <<< Init
        }
    }

profilesByPlayer
    :: forall query children left
    .  String
    -> HH.ComponentHTML
        query (profilesByPlayer :: Slot Unit | children) (Async left)
profilesByPlayer nickname = HH.slot
    (SProxy :: SProxy "profilesByPlayer") unit
    (component nickname) nickname absurd

module TeamTavern.Client.Components.ProfilesByPlayer
    (Slot, profilesByPlayer) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (intercalate)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Foldable (find)
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
import TeamTavern.Server.Profile.ViewByPlayer.SendResponse as ViewByPlayer
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
    [ HH.h3 [ HP.class_ $ ClassName "card-header"] [ HH.text "Profiles" ] ] <>
    (profiles # mapWithIndex \index { handle, title, summary, fieldValues, fields } ->
        HH.div [ HP.class_ $ ClassName "card" ] $ join
        [ pure $
            HH.h3_ [ navigationAnchorIndexed (SProxy :: SProxy "games") index
            { path: "/games/" <> handle, content: HH.text title } ]
        , case nickname' of
            Nothing -> []
            Just nickname -> pure $ HH.p_ [
                HH.a
                [ HP.href ""
                , HE.onClick $ Just <<< ShowEditProfileModal
                    { nickname, handle, title, summary, fieldValues, fields }
                ]
                [ HH.text "Edit profile" ] ]
        , Array.catMaybes $ fields <#> \field -> let
            fieldValue = fieldValues # find \ { fieldId } -> field.id == fieldId
            in
            case { type: field.type, fieldValue } of
            { type: 1, fieldValue: Just { url: Just url' } } -> Just $
                HH.p_
                [ HH.text $ field.label <> ": "
                , HH.a [ HP.href url' ] [ HH.text url' ]
                ]
            { type: 2, fieldValue: Just { optionId: Just optionId' } } -> let
                option' = field.options >>= find (\{ id } -> id == optionId')
                in
                option' <#> \{ option } ->
                    HH.p_ [ HH.text $ field.label <> ": " <> option ]
            { type: 3, fieldValue: Just { optionIds: Just optionIds' } } -> let
                options' = field.options <#> Array.filter \{ id } -> Array.elem id optionIds'
                in
                case options' of
                Just options | not $ Array.null options -> Just $ HH.p_
                    [ HH.text $ field.label <> ": "
                        <> intercalate ", " (options <#> _.option)
                    ]
                _ -> Nothing
            _ ->  Nothing
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

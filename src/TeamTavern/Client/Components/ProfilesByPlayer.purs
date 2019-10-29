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
render (Profiles profiles nickname') =
    HH.div [ HP.class_ $ HH.ClassName "card" ] $
    [ HH.h2 [ HP.class_ $ HH.ClassName "card-title" ] [ HH.text "Profiles" ] ]
    <> case nickname' of
        Nothing -> []
        Just _ -> [ editProfile $ Just <<< HandleEditProfileMessage ]
    <>
    if Array.null profiles
    then pure $
        HH.div [ HP.class_ $ ClassName "card-section" ]
        [ HH.p_ [ HH.text "This player has no game profiles." ] ]
    else
        (profiles # mapWithIndex \index { handle, title, summary, fieldValues, fields } ->
            HH.div [ HP.class_ $ ClassName "card-section" ] $ join
            [ pure $
                HH.h3 [ HP.class_ $ ClassName "profile-title" ] $ join
                    [ pure $ navigationAnchorIndexed (SProxy :: SProxy "games") index
                        { path: "/games/" <> handle, content: HH.text title }
                    , case nickname' of
                        Nothing -> []
                        Just nickname -> pure $
                            HH.button
                                [ HP.class_ $ HH.ClassName "regular-button title-button"
                                , HE.onClick $ Just <<< ShowEditProfileModal
                                    { nickname, handle, title, summary, fieldValues, fields }
                                ]
                                [ HH.i [ HP.class_ $ H.ClassName "fas fa-user-edit button-icon" ] []
                                , HH.text "Edit profile"
                                ]
                    ]
            , Array.catMaybes $ fields <#> \field -> let
                fieldValue = fieldValues # find \ { fieldKey } -> field.key == fieldKey
                in
                case { type: field.type, fieldValue } of
                { type: 1, fieldValue: Just { url: Just url' } } -> Just $
                    HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                    [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                    , HH.a [ HP.class_ $ HH.ClassName "profile-field-label", HP.href url' ] [ HH.text field.label ]
                    ]
                { type: 2, fieldValue: Just { optionKey: Just optionKey' } } -> let
                    option' = field.options >>= find (\{ key } -> key == optionKey')
                    in
                    option' <#> \{ option } ->
                        HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                        [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-label" ] [ HH.text $ field.label <> ": " ]
                        , HH.text option
                        ]
                { type: 3, fieldValue: Just { optionKeys: Just optionKeys' } } -> let
                    fieldOptions' = field.options <#> Array.filter \{ key } -> Array.elem key optionKeys'
                    in
                    case fieldOptions' of
                    Just fieldOptions | not $ Array.null fieldOptions -> Just $
                        HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                        [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-label" ] [ HH.text $ field.label <> ": " ]
                        , HH.text $ intercalate ", " (fieldOptions <#> _.option)
                        ]
                    _ -> Nothing
                _ ->  Nothing
            , summary <#> \paragraph -> HH.p_ [ HH.text paragraph ]
            ])

loadProfiles :: forall left. String -> Async left State
loadProfiles nickname = Async.unify do
    response <-  Fetch.fetch_ ("/api/profiles/by-nickname/" <> nickname)
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

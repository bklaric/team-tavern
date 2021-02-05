module TeamTavern.Client.Components.Pagination (Output(..), Slot, pagination) where

import Prelude

import Data.Const (Const)
import Data.Int (ceil, toNumber)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Script.Url as Url
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame.LoadProfiles (pageSize)
import Web.HTML as Html
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent)

data Output = PageChanged

type Input = { page :: Int, profileCount :: Int }

data State
    = Initial Input
    | Loaded
        { page :: Int
        , totalPages :: Int
        , profileCount :: Int
        , enableBack :: Boolean
        , enableForward :: Boolean
        , firstUrl :: String
        , previousUrl :: String
        , nextUrl :: String
        , lastUrl :: String
        }

data Action = Initialize | Receive Input | ChangePage String MouseEvent

type Slot = H.Slot (Const Void) Output Unit

render :: forall slots. State -> HH.HTML slots Action
render (Initial _) = HH.div_ []
render (Loaded
    { page, totalPages, profileCount, enableBack, enableForward
    , firstUrl, previousUrl, nextUrl, lastUrl
    }) = let
    buildClass enabled class_ =
        class_ <> if enabled then "" else " disabled-pagination-button"
    in
    HH.div [ HS.class_ "pagination" ]
    [ HH.div [ HP.class_$ HH.ClassName "pagination-left-buttons" ]
        [ HH.a
            [ HS.class_ $ buildClass enableBack "pagination-first-button"
            , HP.href firstUrl
            , HE.onClick $ Just <<< ChangePage firstUrl
            ]
            [ HH.text "First" ]
        , HH.a
            [ HS.class_ $ buildClass enableBack "pagination-previous-button"
            , HP.href previousUrl
            , HE.onClick $ Just <<< ChangePage previousUrl
            ]
            [ HH.text "Previous" ]
        ]
    , HH.div [ HS.class_ "pagination-page" ]
        [ HH.text $ show page <> "/" <> show totalPages ]
    , HH.div [ HP.class_$ HH.ClassName "pagination-right-buttons" ]
        [ HH.a
            [ HS.class_ $ buildClass enableForward "pagination-next-button"
            , HP.href nextUrl
            , HE.onClick $ Just <<< ChangePage nextUrl
            ]
            [ HH.text "Next" ]
        , HH.a
            [ HS.class_ $ buildClass enableForward "pagination-last-button"
            , HP.href lastUrl
            , HE.onClick $ Just <<< ChangePage lastUrl
            ]
            [ HH.text "Last" ]
        ]
    ]

inputToLoadedState :: Input -> Effect State
inputToLoadedState { page, profileCount } = do
    let totalPages = ceil (toNumber profileCount / toNumber pageSize)
        enableBack = page /= 1
        enableForward = page /= totalPages

    url <- Html.window >>= Window.location >>= Location.href >>= Url.url
    searchParams <- Url.searchParams url

    Url.set "page" (show 1) searchParams
    firstUrl <- Url.href url

    Url.set "page" (show if enableBack then page - 1 else 1) searchParams
    previousUrl <- Url.href url

    Url.set "page" (show if enableForward then page + 1 else page) searchParams
    nextUrl <- Url.href url

    Url.set "page" (show totalPages) searchParams
    lastUrl <- Url.href url

    pure $ Loaded
        { page, totalPages, profileCount, enableBack, enableForward
        , firstUrl, previousUrl, nextUrl, lastUrl
        }

handleAction :: forall monad slots action. MonadEffect monad =>
    Action -> H.HalogenM State action slots Output monad Unit
handleAction Initialize = do
    state <- H.get
    loadedState <- case state of
        Initial input -> H.liftEffect $ inputToLoadedState input
        Loaded loaded -> pure $ Loaded loaded
    H.put loadedState
handleAction (Receive input) = do
    state <- H.liftEffect $ inputToLoadedState input
    H.put state
handleAction (ChangePage url mouseEvent) =
    navigateWithEvent_ url mouseEvent

component :: forall monad query. MonadEffect monad => H.Component HH.HTML query Input Output monad
component = H.mkComponent
    { initialState: Initial
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

pagination
    :: forall monad action children
    .  MonadEffect monad
    => Int
    -> Int
    -> (Output -> action)
    -> HH.ComponentHTML action (pagination :: Slot | children) monad
pagination page profileCount onMessage =
    HH.slot (SProxy :: SProxy "pagination") unit component
    { page, profileCount } (Just <<< onMessage)

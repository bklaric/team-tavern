module TeamTavern.Client.Pages.DeleteAlert where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Script.Request (deleteNoContent)
import TeamTavern.Client.Script.Url as Url
import TeamTavern.Client.Snippets.Class as HS
import Web.HTML as Html
import Web.HTML.Location as Location
import Web.HTML.Window as Window

data Action = Initialize

data State = Empty | Deleted | Error

type Slot = H.Slot (Const Void) Void Unit

render :: forall slots action. State -> HH.HTML slots action
render Empty = HH.div_ []
render Deleted = HH.div [ HS.class_ "single-message" ] [ HH.p_ [ HH.text "Alert has been successfully removed." ] ]
render Error =
    HH.div [ HS.class_ "single-message" ]
    [ HH.p_ $
        [ HH.text "There has been an error removing the alert. "
        , HH.text "Please try again later or contact "
        , HH.a [ HP.href "mailto:admin@teamtavern.net" ] [ HH.text "admin@teamtavern.net" ]
        , HH.text " for help."
        ]
    ]

handleAction :: forall slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction Initialize = do
    searchParams <- Html.window >>= Window.location >>= Location.href
        >>= Url.url >>= Url.searchParams # liftEffect
    alertToken' <- Url.get "alertToken" searchParams # liftEffect
    case alertToken' of
        Nothing -> H.put Error
        Just alertToken -> do
            result <- H.lift $ deleteNoContent $ "/alerts/" <> alertToken
            case result of
                Nothing -> H.put Deleted
                Just _ -> H.put Error

component :: forall query input output left. H.Component HH.HTML query input output (Async left)
component = H.mkComponent
    { initialState: const Empty
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }

deleteAlert :: forall query children left.
    HH.ComponentHTML query (deleteAlert :: Slot | children) (Async left)
deleteAlert = HH.slot (SProxy :: _ "deleteAlert") unit component unit absurd

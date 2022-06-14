module TeamTavern.Client.Pages.DeleteAlert where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Type.Proxy (Proxy(..))
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Script.Url as Url
import TeamTavern.Client.Snippets.Class as HS
import Web.HTML as Html
import Web.HTML.Location as Location
import Web.HTML.Window as Window

data Action = Initialize

data State = Empty | Deleted | NotFound | Error

type Slot = H.Slot (Const Void) Void Unit

render :: forall slots action. State -> HH.HTML slots action
render Empty = HH.div_ []
render Deleted =
    HH.div [ HS.class_ "single-message" ]
    [ HH.p_ [ HH.text "Alert has been successfully removed." ] ]
render NotFound =
    HH.div [ HS.class_ "single-message" ]
    [ HH.p_ $
        [ HH.text "The alert you are trying to remove doesn't seem to exist. "
        , HH.text "If you are still receiving alert emails, please contact "
        , HH.a [ HP.href "mailto:admin@teamtavern.net" ] [ HH.text "admin@teamtavern.net" ]
        , HH.text " for help."
        ]
    ]
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
    id' <- Url.get "id" searchParams # liftEffect
    token' <- Url.get "token" searchParams # liftEffect
    case id', token' of
        Just id, Just token -> do
            nextState <- H.lift $ Async.unify do
                let url = "/api/alerts/" <> id <> "?token=" <> token
                response <- Fetch.fetch url (Fetch.method := DELETE) # lmap (const Error)
                case FetchRes.status response of
                    204 -> pure Deleted
                    404 -> pure NotFound
                    _ -> pure Error
            H.put nextState
        _, _ -> H.put Error

component :: forall query input output left. H.Component query input output (Async left)
component = H.mkComponent
    { initialState: const Empty
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }

deleteAlert :: forall query children left.
    HH.ComponentHTML query (deleteAlert :: Slot | children) (Async left)
deleteAlert = HH.slot (Proxy :: _ "deleteAlert") unit component unit absurd

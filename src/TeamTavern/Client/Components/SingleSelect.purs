module TeamTavern.Client.Components.SingleSelect where

import Prelude

import Async (Async(..))
import Data.Const (Const(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)
import Unsafe.Reference (unsafeRefEq)
import Web.Event.Event (target)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (fromEventTarget, setClassName)
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Option = { id :: Int, option :: String }

type State =
    { options :: Array Option
    , selected :: Maybe Option
    , optionsShown :: Boolean
    }

data Action = Select (Maybe Option) | Toggle

data Query send = Selected ((Maybe Option) -> send)

type Slot = H.Slot Query Void

render { options, selected, optionsShown } =
    HH.div
    [ HP.class_ $ ClassName "select" ]
    $ [ HH.div
        [ HP.class_ $ ClassName
            if optionsShown then "selected-open" else "selected"
        , HE.onMouseDown $ const $ Just $ Toggle
        ]
        [ HH.text
            case selected of
            Nothing -> ""
            Just { option } -> option
        ]
    ]
    <> if optionsShown
        then
            [ HH.div
            [ HP.class_ $ ClassName "options" ]
            $ [ HH.div
                [ HP.class_ $ ClassName "option"
                , HE.onClick $ const $ Just $ Select Nothing
                ]
                [ HH.text "" ]
            ]
            <>
            (options <#> \option ->
                HH.div
                [ HP.class_ $ ClassName "option"
                , HE.onClick $ const $ Just $ Select $ Just $ option
                ]
                [ HH.text option.option ])
            ]
        else
            []

handleAction :: forall slots message left.
    Action -> H.HalogenM State Action slots message (Async left) Unit
handleAction (Select selected) =
    H.modify_ (_ { selected = selected, optionsShown = false })
handleAction Toggle =
    H.modify_ \state -> state { optionsShown = not state.optionsShown }

handleQuery (Selected send) = do
    { selected } <- H.get
    pure $ Just $ send selected

component :: forall message left.
    H.Component HH.HTML Query (Array Option) message (Async left)
component = H.mkComponent
    { initialState: { options: _, selected: Nothing, optionsShown: false }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }

singleSelect label input = HH.slot label unit component input absurd

singleSelectIndexed label index input =
    HH.slot label index component input absurd

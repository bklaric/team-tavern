module TeamTavern.Client.Components.MultiSelect where

import Prelude

import Async (Async(..))
import Async as Async
import Data.Array (elem, snoc)
import Data.Array as Array
import Data.Const (Const(..))
import Data.Foldable (find, intercalate)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)
import Unsafe.Reference (unsafeRefEq)
import Web.Event.Event (target)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLDocument as Document
import Web.HTML.HTMLElement (fromEventTarget, setClassName)
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import Web.UIEvent.MouseEvent.EventTypes (click)

type Option option =
    { option :: option
    , selected :: Boolean
    }

type Input option =
    { options :: Array (Option option)
    , labeler :: option -> String
    , comparer :: option -> option -> Boolean
    }

type State option =
    { options :: Array (Option option)
    , labeler :: option -> String
    , comparer :: option -> option -> Boolean
    , open :: Boolean
    }

data Action option
    = ToggleOption option
    | Open
    | Close
    | Toggle

data Query option send = Selected (Array option -> send)

type Slot option = H.Slot (Query option) Void

render { options, labeler, comparer, open } =
    HH.div
    [ HP.class_ $ ClassName "select"
    , HP.tabIndex 0
    , HE.onFocus $ const $ Just Open
    , HE.onBlur $ const $ Just Close
    ]
    $ [ HH.div
        [ HP.class_ $ ClassName
            if open then "selected-open" else "selected"
        , HE.onMouseDown $ const $ Just Toggle
        ]
        [ HH.text $ intercalate ", "
            (options # Array.filter (_.selected) <#> (_.option >>> labeler))
        ]
    ]
    <> if open
        then
            [ HH.div
                [ HP.class_ $ ClassName "options" ]
                (options <#> \{ option, selected } ->
                    HH.div
                    [ HP.class_ $ ClassName "option"
                    , HE.onClick $ const $ Just $ ToggleOption option
                    ]
                    [ HH.input
                        [ HP.type_ InputCheckbox
                        , HP.checked selected
                        , HP.tabIndex $ -1
                        ]
                    , HH.text option.option
                    ])
            ]
        else
            []

handleAction :: forall option slots message left.
    (Action option) -> H.HalogenM (State option) (Action option) slots message (Async left) Unit
handleAction (ToggleOption toggledOption) =
    H.modify_ (\state -> state
        { options = state.options <#> \{ option, selected } ->
            if state.comparer option toggledOption
            then { option, selected: not selected }
            else { option, selected }
        })
handleAction Open =
    H.modify_ \state -> state { open = true }
handleAction Close =
    H.modify_ \state -> state { open = false }
handleAction Toggle =
    H.modify_ \state -> state { open = not state.open }

handleQuery (Selected send) = do
    { options } <- H.get
    pure $ Just $ send (options <#> _.option)

-- component :: forall option message left.
--     H.Component HH.HTML (Query option) (Input option) message (Async left)
component = H.mkComponent
    { initialState: \{ options, labeler, comparer } ->
        { options, labeler, comparer, open: false }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }

multiSelect label input = HH.slot label unit component input absurd

multiSelectIndexed label index input =
    HH.slot label index component input absurd

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

type Option = { id :: Int, option :: String }

type Input =
    { options :: Array Option
    , selectedIds :: Array Int
    }

type State =
    { options :: Array Option
    , selected :: Array Option
    , optionsShown :: Boolean
    }

data Action = Select Option | Deselect Option | Open | Close | Toggle

data Query send = Selected ((Array Option) -> send)

type Slot = H.Slot Query Void

optionIsSelected options option =
    options
    # find (\{ id } -> id == option.id)
    # isJust

render { options, selected, optionsShown } =
    HH.div
    [ HP.class_ $ ClassName "select"
    , HP.tabIndex 0
    , HE.onFocus $ const $ Just Open
    , HE.onBlur $ const $ Just Close
    ]
    $ [ HH.div
        [ HP.class_ $ ClassName
            if optionsShown then "selected-open" else "selected"
        , HE.onMouseDown $ const $ Just Toggle
        ]
        [ HH.text $ intercalate ", " (selected # Array.sortWith _.id <#> _.option )]
    ]
    <> if optionsShown
        then
            [ HH.div
                [ HP.class_ $ ClassName "options" ]
                (options <#> \option -> let
                    isSelected = optionIsSelected selected option
                    in
                    HH.div
                    [ HP.class_ $ ClassName "option"
                    , HE.onClick $ const $ Just $ (if isSelected then Deselect else Select) option
                    ]
                    [ HH.input
                        [ HP.type_ InputCheckbox
                        , HP.checked isSelected
                        ]
                    , HH.text option.option
                    ])
            ]
        else
            []

handleAction :: forall slots message left.
    Action -> H.HalogenM State Action slots message (Async left) Unit
handleAction (Select option) =
    H.modify_ (\state -> state { selected = Array.snoc state.selected option })
handleAction (Deselect option) =
    H.modify_ (\state -> state { selected = state.selected # Array.filter \{ id } -> id /= option.id })
handleAction Open =
    H.modify_ \state -> state { optionsShown = true }
handleAction Close =
    H.modify_ \state -> state { optionsShown = false }
handleAction Toggle =
    H.modify_ \state -> state { optionsShown = not state.optionsShown }

handleQuery (Selected send) = do
    { selected } <- H.get
    pure $ Just $ send selected

component :: forall message left.
    H.Component HH.HTML Query Input message (Async left)
component = H.mkComponent
    { initialState: \{ options, selectedIds } ->
        { options
        , selected: options # Array.filter (\{ id } -> Array.elem id selectedIds)
        , optionsShown: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }

multiSelect label input = HH.slot label unit component input absurd

multiSelectIndexed label index input =
    HH.slot label index component input absurd

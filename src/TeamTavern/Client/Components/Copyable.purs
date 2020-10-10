module Client.Components.Copyable (Input, Slot, copyable) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prim.Row (class Cons)
import TeamTavern.Client.Script.Clipboard as Clipboard
import TeamTavern.Client.Snippets.Class as HS

type Input = String

type State = { text :: String, copied :: Boolean }

data Action = CopyText | Receive Input

type Slot = H.Slot (Const Void) Void Unit

render :: forall children. State -> HH.HTML children Action
render { text, copied } =
    HH.span_ $
    [ HH.span
        [ HS.class_ "copyable"
        , HE.onClick $ const $ Just CopyText
        ] $
        [ HH.text text ]
    ]
    <>
    if copied
    then Array.singleton $
        HH.span [ HS.class_ "copyable-copied" ] [ HH.text "Copied!" ]
    else []

handleAction :: forall children output left.
    Action -> H.HalogenM State Action children output (Async left) Unit
handleAction CopyText = do
    { text } <- H.get
    result <- H.lift $ Async.attempt $ Clipboard.writeTextAsync text
    case result of
        Right _ -> H.modify_ _ { copied = true }
        _ -> pure unit
handleAction (Receive text) =
    H.put { text, copied: false }

component :: forall output left query.
    H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: { text: _, copied: false }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

copyable
    :: forall children' name children action left
    .  Cons name Slot children' children
    => IsSymbol name
    => SProxy name
    -> Input
    -> HH.ComponentHTML action children (Async left)
copyable label text = HH.slot label unit component text absurd
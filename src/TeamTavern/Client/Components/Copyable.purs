module Client.Components.Copyable (Input, copyable) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prim.Row (class Cons)
import TeamTavern.Client.Script.Analytics (track)
import TeamTavern.Client.Script.Clipboard as Clipboard
import TeamTavern.Client.Shared.Slot (Slot__I)
import TeamTavern.Client.Snippets.Class as HS
import Type.Proxy (Proxy)

type Input = String

type State = { text :: String, copied :: Boolean }

data Action = CopyText | Receive Input

render :: ∀ children. State -> HH.HTML children Action
render { text, copied } =
    HH.span_ $
    [ HH.span
        [ HS.class_ "copyable"
        , HE.onClick $ const CopyText
        ] $
        [ HH.text text ]
    ]
    <>
    if copied
    then Array.singleton $
        HH.span [HS.class_ "copyable-copied"] [HH.text "Copied!"]
    else Array.singleton $
        HH.span [HS.class_ "copyable-copy"] [HH.text "Copy"]

handleAction :: ∀ children output left.
    Action -> H.HalogenM State Action children output (Async left) Unit
handleAction CopyText = do
    { text } <- H.get
    result <- H.lift $ Async.attempt $ Clipboard.writeTextAsync text
    case result of
        Right _ -> do
            H.modify_ _ { copied = true }
            track "Copyable click" {text}
        _ -> pure unit
handleAction (Receive text) =
    H.put { text, copied: false }

component :: ∀ output left query.
    H.Component query Input output (Async left)
component = H.mkComponent
    { initialState: { text: _, copied: false }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

copyable
    :: ∀ index children' name children action left
    .  Cons name (Slot__I index) children' children
    => IsSymbol name
    => Ord index
    => Proxy name
    -> index
    -> Input
    -> HH.ComponentHTML action children (Async left)
copyable label index text = HH.slot label index component text absurd

module TeamTavern.Client.Components.Textarea (Text, Query(..), Message(..), textarea) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (any, foldl)
import Data.Array as Arary
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.HTTP.Method (Method(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Options ((:=))
import Data.String (null)
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy(..), match)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.MultiSelect (multiSelectIndexed)
import TeamTavern.Client.Components.MultiSelect as MultiSelect
import TeamTavern.Client.Components.SingleSelect (singleSelectIndexed)
import TeamTavern.Client.Components.SingleSelect as SingleSelect
import TeamTavern.Client.Game.GameHeader as GameHeader
import TeamTavern.Client.Script.Cookie (PlayerInfo)
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Server.Game.View.SendResponse as View
import TeamTavern.Server.Profile.AddPlayerProfile.SendResponse as Create
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Text = String

type Input = { text :: Text, placeholder :: Text }

data Action = TextInput Text

type State = Text

data Query send = GetText (Text -> send) | Clear send

data Message = TextChanged Text

render { text, placeholder } = HH.textarea
    [ HP.class_ $ HH.ClassName "text-input"
    , HP.placeholder placeholder
    , HP.value text
    , HE.onValueInput $ Just <<< TextInput
    ]

handleAction (TextInput text) = do
    H.modify_ (_ { text = text })
    H.raise $ TextChanged text

handleQuery (GetText send) = do
    { text } <- H.get
    pure $ Just $ send text
handleQuery (Clear send) = do
    H.modify_ (_ { text = "" })
    pure $ Just send

component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }

textarea label input handleMessage =
    HH.slot label unit component input handleMessage

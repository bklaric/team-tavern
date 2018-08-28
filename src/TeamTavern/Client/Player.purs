module TeamTavern.Client.Player where

import Prelude

import Async (Async)
import Async as A
import Browser.Async.Fetch as FA
import Browser.Async.Fetch.Response as FARes
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Effect.Class.Console (log, logShow)
import Error.Class as E
import Halogen as H
import Halogen.HTML as HH
import Simple.JSON as J

data Query send = Init send

data State
    = Nickname String
    | View { nickname :: String, about :: String }
    | NotFound String
    | Error String

type Message = Void

type Slot = H.Slot Query Message

type ChildSlots = ()

render :: forall m. State -> H.ComponentHTML Query ChildSlots m
render (Nickname nickname) = HH.p_
    [ HH.text $ "Loading player " <> nickname ]
render (View { nickname, about }) = HH.div_
    [ HH.h2_ [ HH.text nickname ]
    , HH.p_ [ HH.text about ]
    ]
render (NotFound nickname) = HH.p_
    [ HH.text $ "Player " <> nickname <> " could not be found" ]
render (Error nickname) = HH.p_
    [ HH.text $ "Error loading player " <> nickname ]

eval :: forall void.
    Query ~> H.HalogenM State Query ChildSlots Message (Async void)
eval (Init send) = do
    state <- H.get
    newState <- case state of
        Nickname nickname -> do
            response' <- H.lift $ A.attempt $ FA.fetch
                ("http://localhost:8080/players/by-nickname/" <> nickname)
                (FA.method := GET)
            case response' of
                Left error -> do
                    log $ E.message error
                    pure $ Error nickname
                Right response -> H.lift
                    case FARes.status response of
                    200 -> FARes.text response <#> J.readJSON >>=
                        case _ of
                        Left errors -> do
                            logShow errors
                            pure $ Error nickname
                        Right view -> pure $ View view
                    404 -> pure $ NotFound nickname
                    _ -> pure $ Error nickname
        _ -> pure state
    H.put newState
    pure send

player :: forall void. H.Component HH.HTML Query String Message (Async void)
player = H.component
    { initialState: Nickname
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Init
    , finalizer: Nothing
    }

module TeamTavern.Client.CreateProfile where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (bimap, lmap)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.String (trim)
import Data.Variant (SProxy(..), match)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Script.Cookie (hasPlayerIdCookie)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Snippets.ErrorClasses (errorClass, inputErrorClass, otherErrorClass)
import TeamTavern.Profile.Create.Response as Create
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data Query send
    = Init send
    | SummaryInput String send
    | Create Event send

type State =
    { handle :: String
    , summary :: String
    , summaryError :: Boolean
    , otherError :: Boolean
    }

type Slot = H.Slot Query Void

render :: forall left. State -> H.ComponentHTML Query () (Async left)
render { summary, summaryError, otherError } = HH.form
    [ HE.onSubmit $ HE.input Create ]
    [ HH.h2_ [ HH.text "Create a new profile" ]
    , HH.div_
        [ HH.label
            [ HP.class_ $ errorClass summaryError, HP.for "summary" ]
            [ HH.text "Summary" ]
        , HH.textarea
            [ HP.id_ "summary"
            , HP.class_ $ errorClass summaryError
            , HE.onValueInput $ HE.input SummaryInput
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass summaryError ]
            [ HH.text "This does not look like a valid summary. Jesus Christ, how dense are you?" ]
        ]
    , HH.button
        [ HP.class_ $ ClassName "primary"
        , HP.disabled $ summary == ""
        ]
        [ HH.text "Create" ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Lmao, something else got fucked and you're shit out of luck, mate!"]
    ]

sendCreateRequest :: forall left. State -> Async left (Maybe State)
sendCreateRequest state @ { handle, summary } = Async.unify do
    response <- Fetch.fetch
        ("/api/games/" <> handle <> "/profiles")
        (  Fetch.method := POST
        <> Fetch.body := Json.writeJSON { summary }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true })
                (\(error :: Create.BadRequestContent) -> Just $ match
                    { invalidSummary: const $ state { summaryError = true }
                    }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

eval :: forall void. Query ~> H.HalogenM State Query () Void (Async void)
eval (Init send) = do
    isSignedIn <- H.liftEffect hasPlayerIdCookie
    if isSignedIn
        then pure unit
        else H.liftEffect $ navigate_ "/"
    pure send
eval (SummaryInput summary send) =
    H.modify_ (_ { summary = summary }) <#> const send
eval (Create event send) = do
    H.liftEffect $ preventDefault event
    state <- H.gets (_
        { summaryError = false
        , otherError   = false
        })
    newState <- H.lift $ sendCreateRequest state
    case newState of
        Nothing -> H.liftEffect $ navigate_ $ "/games/" <> trim state.handle
        Just newState' -> H.put newState'
    pure send

component :: forall input left.
    String -> H.Component HH.HTML Query input Void (Async left)
component handle = H.component
    { initialState: const
        { handle
        , summary: ""
        , summaryError: false
        , otherError: false
        }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Init
    , finalizer: Nothing
    }

createProfile
    :: forall query children left
    .  String
    -> HH.ComponentHTML query
        (createProfile :: Slot Unit | children)
        (Async left)
createProfile handle =
    HH.slot
        (SProxy :: SProxy "createProfile") unit (component handle) unit absurd

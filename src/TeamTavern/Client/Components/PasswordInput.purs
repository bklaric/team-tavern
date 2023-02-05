module TeamTavern.Client.Components.PasswordInput where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..), InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Prim.Row (class Cons)
import TeamTavern.Client.Shared.Slot (Slot_OI)
import TeamTavern.Client.Snippets.Class as HS
import Type.Proxy (Proxy(..))

component :: forall q m. H.Component q String String m
component = Hooks.component \{outputToken} password -> Hooks.do
    shown /\ shownId <- Hooks.useState false
    Hooks.pure $
        HH.div [ HS.class_ "password-input-container" ]
        [ HH.input
            [ HP.id "password"
            , HS.class_ "password-input"
            , HP.type_
                if shown
                then InputText
                else InputPassword
            , HP.name "password"
            , HP.value password
            , HE.onValueInput $ Hooks.raise outputToken
            ]
        , HH.button
            [ HS.class_ "password-input-button"
            , HP.type_ ButtonButton
            , HP.title
                if shown
                then "Hide password"
                else "Show password"
            , HP.tabIndex (-1)
            , HE.onClick $ const $ Hooks.modify_ shownId not
            ]
            [ HH.i
                [ HS.class_
                    if shown
                    then "fas fa-eye-slash"
                    else "fas fa-eye"
                ]
                []
            ]
        ]

passwordInput :: forall action slots m label slots'.
    Cons label (Slot_OI String Unit) slots' slots => IsSymbol label =>
    Proxy label -> String -> (String -> action) -> HH.ComponentHTML action slots m
passwordInput label input onOutput = HH.slot label unit component input onOutput

passwordInput_ :: forall action m slots.
    String -> (String -> action) -> HH.ComponentHTML action (passwordInput :: Slot_OI String Unit | slots) m
passwordInput_ = passwordInput (Proxy :: _ "passwordInput")

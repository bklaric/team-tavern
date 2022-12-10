module TeamTavern.Client.Components.PasswordInput where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..), InputType(..))
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Class as HS

passwordInput :: ∀ slots action.
    String -> Boolean -> (String -> action) -> action -> HH.HTML slots action
passwordInput password passwordShown onPassword togglePasswordVisibility =
    HH.div [ HS.class_ "password-input-container" ]
    [ HH.input
        [ HP.id "password"
        , HS.class_ "password-input"
        , HP.type_
            if passwordShown
            then InputText
            else InputPassword
        , HP.value password
        , HE.onValueInput onPassword
        ]
    , HH.button
        [ HS.class_ "password-input-button"
        , HP.type_ ButtonButton
        , HP.title
            if passwordShown
            then "Hide password"
            else "Show password"
        , HE.onClick $ const togglePasswordVisibility
        ]
        [ HH.i
            [ HS.class_
                if passwordShown
                then "fas fa-eye-slash"
                else "fas fa-eye"
            ]
            []
        ]
    ]

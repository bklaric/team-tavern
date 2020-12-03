module TeamTavern.Client.Snippets.ErrorClasses where

import Halogen (ClassName(..))

inputErrorClass :: Boolean -> ClassName
inputErrorClass hasError = ClassName
    if hasError then "input-error" else "hidden-error"

otherErrorClass :: Boolean -> ClassName
otherErrorClass hasError = ClassName
    if hasError then "form-error" else "hidden-error"

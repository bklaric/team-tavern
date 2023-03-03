module TeamTavern.Client.Components.InputError where

import Halogen.HTML as HH
import TeamTavern.Client.Components.Input (inputError)

nicknameError :: forall slots action. Boolean -> Array (HH.HTML slots action)
nicknameError shown = inputError shown
    """Nickname cannot be more than 40 characters long and can contain
    only alphanumeric characters, dashes, underscores and dots."""

nicknameTaken :: forall slots action. Boolean -> Array (HH.HTML slots action)
nicknameTaken shown = inputError shown
    "This nickname is already taken, please pick another one."

passwordError :: forall slots action. Boolean -> Array (HH.HTML slots action)
passwordError shown = inputError shown
    "Password must have at least 8 characters."

passwordWrong :: forall slots action. Boolean -> Array (HH.HTML slots action)
passwordWrong shown = inputError shown
    "Entered password is incorrect."

emailError :: forall slots action. Boolean -> Array (HH.HTML slots action)
emailError shown = inputError shown
    "This doesn't look like a valid email address."

emailTaken :: forall slots action. Boolean -> Array (HH.HTML slots action)
emailTaken shown = inputError shown
    "This email is already taken, please pick another one."

discordTaken :: forall slots action. Boolean -> Array (HH.HTML slots action)
discordTaken shown = inputError shown
    """An account already exists for this Discord user.
    Try signin in instead."""

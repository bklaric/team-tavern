module TeamTavern.Routes.Shared.Wait where

-- | How long until a request refused as one too many would be let through.
type Wait = { seconds :: Int }

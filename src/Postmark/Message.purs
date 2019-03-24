module Postmark.Message where

import Data.Nullable (Nullable)

type Header =
    { name :: String
    , value :: String
    }

type Attachment =
    { content :: String
    , name :: String
    , contentType :: String
    }

type Message =
    { to :: String
    , from :: String
    -- , cc :: Nullable String
    -- , bcc :: Nullable String
    -- , replyTo :: Nullable String
    -- , tag :: Nullable String
    , subject :: Nullable String
    , htmlBody :: Nullable String
    , textBody :: Nullable String
    -- , trackOpens :: Nullable Boolean
    -- , trackLinks :: Nullable String
    -- , headers :: Nullable (Array Header)
    -- , attachments :: Nullable (Array Attachment)
    }

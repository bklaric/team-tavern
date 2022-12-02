module Jarilo.Types where

-- Method

foreign import data Method :: Type

foreign import data Options :: Method

foreign import data Head :: Method

foreign import data Get :: Method

foreign import data Post :: Method

foreign import data Put :: Method

foreign import data Patch :: Method

foreign import data Delete :: Method

-- Path

foreign import data Path :: Type

foreign import data Literal :: Symbol -> Path

foreign import data Capture :: Symbol -> Type -> Path

foreign import data PathChain :: Path -> Path -> Path

infixr 9 type PathChain as :>

-- Query

foreign import data Query :: Type

foreign import data NoQuery :: Query

foreign import data Optional :: Symbol -> Type -> Query

foreign import data Mandatory :: Symbol -> Type -> Query

foreign import data Many :: Symbol -> Type -> Query

foreign import data Rest :: Symbol -> Query

foreign import data QueryChain :: Query -> Query -> Query

infixr 9 type QueryChain as :?

-- Body

foreign import data Body :: Type

foreign import data NoBody :: Body

foreign import data JsonBody :: Type -> Body

foreign import data FormBody :: Body

-- Request

foreign import data Request :: Type

foreign import data FullRequest :: Method -> Path -> Query -> Body -> Request

-- Status

foreign import data Status :: Type

foreign import data Ok :: Status

foreign import data NoContent :: Status

foreign import data BadRequest :: Status

foreign import data NotAuthorized :: Status

foreign import data Forbidden :: Status

foreign import data Internal :: Status

-- Response

foreign import data Response :: Type

foreign import data FullResponse :: Status -> Body -> Response

foreign import data ResponseChain :: Response -> Response -> Response

infixr 9 type ResponseChain as :!

-- Route

foreign import data Route :: Type

foreign import data FullRoute :: Request -> Response -> Route

-- Junction

foreign import data Junction :: Type

foreign import data NamedRoute :: Symbol -> Route -> Junction

infixr 9 type NamedRoute as :=

foreign import data JunctionChain :: Junction -> Junction -> Junction

infixr 8 type JunctionChain as :<|>

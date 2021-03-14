module TeamTavern.Routes.Shared.Organization where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (except)
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), Variant, inj, prj)
import Foreign (ForeignError(..), readString, unsafeToForeign)
import Jarilo.FromComponent (class FromComponent)
import Prim.RowList (class RowToList)
import Simple.JSON (class ReadForeign, class ReadForeignVariant, class WriteForeign, readVariantImpl, writeImpl)
import Type.Data.RowList (RLProxy(..))

-- Organization enum.

data Organization = Informal | Organized

toOrganizationN :: Organization -> OrganizationN
toOrganizationN Informal = Informal'
toOrganizationN Organized = Organized' { name: "" }

toOrganizationNW :: Organization -> OrganizationNW
toOrganizationNW Informal = Informal''
toOrganizationNW Organized = Organized'' { name: "", website: Nothing }

derive instance eqOrganization :: Eq Organization

fromString :: String -> Maybe Organization
fromString "informal" = Just Informal
fromString "organized" = Just Organized
fromString _ = Nothing

fromString' :: String -> Either String Organization
fromString' organization = fromString organization # note ("Unknown organization: " <> organization)

toString :: Organization -> String
toString Informal = "informal"
toString Organized = "organized"

instance readForeignOrganization :: ReadForeign Organization where
    readImpl organization' =
        readString organization'
        >>= (fromString' >>> lmap (ForeignError >>> NonEmptyList.singleton) >>> except)

instance writeForeignOrganization :: WriteForeign Organization where
    writeImpl organization = unsafeToForeign $ toString organization

instance fromComponentOrganization :: FromComponent Organization where
    fromComponent organization = fromString' organization

-- Organization with name.

data OrganizationN = Informal' | Organized' { name :: String }

fromOrganizationN :: OrganizationN -> Organization
fromOrganizationN Informal' = Informal
fromOrganizationN (Organized' _) = Organized

nameOrHandleN :: String -> OrganizationN -> String
nameOrHandleN handle Informal' = handle
nameOrHandleN _ (Organized' { name }) = name

type OrganizationRow' = (informal :: {}, organized :: { name :: String })

fromVariantN :: Variant OrganizationRow' -> Maybe OrganizationN
fromVariantN variant =
    (Informal' <$ prj (SProxy :: _ "informal") variant)
    <|> (Organized' <$> prj (SProxy :: _ "organized") variant)

fromVariantN' :: Variant OrganizationRow' -> Either String OrganizationN
fromVariantN' organization = fromVariantN organization # note ("Unknown organization: " <> show organization)

toVariantN :: OrganizationN -> Variant OrganizationRow'
toVariantN Informal' = inj (SProxy :: _ "informal") {}
toVariantN (Organized' stuff) = inj (SProxy :: _ "organized") stuff

instance readForeignOrganizationN ::
    ( RowToList OrganizationRow' rowList
    , ReadForeignVariant rowList OrganizationRow'
    ) => ReadForeign OrganizationN where
    readImpl organization' =
        readVariantImpl (RLProxy :: _ rowList) organization'
        >>= (fromVariantN' >>> lmap (ForeignError >>> NonEmptyList.singleton) >>> except)

instance writeForeignOrganizationN :: WriteForeign OrganizationN where
    writeImpl organization = writeImpl $ toVariantN organization

-- Organization with name and website.

data OrganizationNW = Informal'' | Organized'' { name :: String, website :: Maybe String }

fromOrganizationNW :: OrganizationNW -> Organization
fromOrganizationNW Informal'' = Informal
fromOrganizationNW (Organized'' _) = Organized

nameOrHandleNW :: String -> OrganizationNW -> String
nameOrHandleNW handle Informal'' = handle
nameOrHandleNW _ (Organized'' { name }) = name

websiteNW :: OrganizationNW -> Maybe String
websiteNW Informal'' = Nothing
websiteNW (Organized'' { website }) = website

type OrganizationRow'' = (informal :: {}, organized :: { name :: String, website :: Maybe String })

fromVariantNW :: Variant OrganizationRow'' -> Maybe OrganizationNW
fromVariantNW variant =
    (Informal'' <$ prj (SProxy :: _ "informal") variant)
    <|> (Organized'' <$> prj (SProxy :: _ "organized") variant)

fromVariantNW' :: Variant OrganizationRow'' -> Either String OrganizationNW
fromVariantNW' organization = fromVariantNW organization # note ("Unknown organization: " <> show organization)

toVariantNW :: OrganizationNW -> Variant OrganizationRow''
toVariantNW Informal'' = inj (SProxy :: _ "informal") {}
toVariantNW (Organized'' stuff) = inj (SProxy :: _ "organized") stuff

instance readForeignOrganizationNW ::
    ( RowToList OrganizationRow'' rowList
    , ReadForeignVariant rowList OrganizationRow''
    ) => ReadForeign OrganizationNW where
    readImpl organization' =
        readVariantImpl (RLProxy :: _ rowList) organization'
        >>= (fromVariantNW' >>> lmap (ForeignError >>> NonEmptyList.singleton) >>> except)

instance writeForeignOrganizationNW :: WriteForeign OrganizationNW where
    writeImpl organization = writeImpl $ toVariantNW organization

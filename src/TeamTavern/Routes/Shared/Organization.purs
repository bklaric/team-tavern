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
toOrganizationN Informal = InformalN
toOrganizationN Organized = OrganizedN { name: "" }

toOrganizationNW :: Organization -> OrganizationNW
toOrganizationNW Informal = InformalNW
toOrganizationNW Organized = OrganizedNW { name: "", website: Nothing }

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

data OrganizationN = InformalN | OrganizedN { name :: String }

fromOrganizationN :: OrganizationN -> Organization
fromOrganizationN InformalN = Informal
fromOrganizationN (OrganizedN _) = Organized

nameOrHandleN :: String -> OrganizationN -> String
nameOrHandleN handle InformalN = handle
nameOrHandleN _ (OrganizedN { name }) = name

type OrganizationRowN = (informal :: {}, organized :: { name :: String })

fromVariantN :: Variant OrganizationRowN -> Maybe OrganizationN
fromVariantN variant =
    (InformalN <$ prj (SProxy :: _ "informal") variant)
    <|> (OrganizedN <$> prj (SProxy :: _ "organized") variant)

fromVariantN' :: Variant OrganizationRowN -> Either String OrganizationN
fromVariantN' organization = fromVariantN organization # note ("Unknown organization: " <> show organization)

toVariantN :: OrganizationN -> Variant OrganizationRowN
toVariantN InformalN = inj (SProxy :: _ "informal") {}
toVariantN (OrganizedN stuff) = inj (SProxy :: _ "organized") stuff

instance readForeignOrganizationN ::
    ( RowToList OrganizationRowN rowList
    , ReadForeignVariant rowList OrganizationRowN
    ) => ReadForeign OrganizationN where
    readImpl organization' =
        readVariantImpl (RLProxy :: _ rowList) organization'
        >>= (fromVariantN' >>> lmap (ForeignError >>> NonEmptyList.singleton) >>> except)

instance writeForeignOrganizationN :: WriteForeign OrganizationN where
    writeImpl organization = writeImpl $ toVariantN organization

-- Organization with name and website.

data OrganizationNW = InformalNW | OrganizedNW { name :: String, website :: Maybe String }

fromOrganizationNW :: OrganizationNW -> Organization
fromOrganizationNW InformalNW = Informal
fromOrganizationNW (OrganizedNW _) = Organized

nameOrHandleNW :: String -> OrganizationNW -> String
nameOrHandleNW handle InformalNW = handle
nameOrHandleNW _ (OrganizedNW { name }) = name

websiteNW :: OrganizationNW -> Maybe String
websiteNW InformalNW = Nothing
websiteNW (OrganizedNW { website }) = website

type OrganizationRowNW = (informal :: {}, organized :: { name :: String, website :: Maybe String })

fromVariantNW :: Variant OrganizationRowNW -> Maybe OrganizationNW
fromVariantNW variant =
    (InformalNW <$ prj (SProxy :: _ "informal") variant)
    <|> (OrganizedNW <$> prj (SProxy :: _ "organized") variant)

fromVariantNW' :: Variant OrganizationRowNW -> Either String OrganizationNW
fromVariantNW' organization = fromVariantNW organization # note ("Unknown organization: " <> show organization)

toVariantNW :: OrganizationNW -> Variant OrganizationRowNW
toVariantNW InformalNW = inj (SProxy :: _ "informal") {}
toVariantNW (OrganizedNW stuff) = inj (SProxy :: _ "organized") stuff

instance readForeignOrganizationNW ::
    ( RowToList OrganizationRowNW rowList
    , ReadForeignVariant rowList OrganizationRowNW
    ) => ReadForeign OrganizationNW where
    readImpl organization' =
        readVariantImpl (RLProxy :: _ rowList) organization'
        >>= (fromVariantNW' >>> lmap (ForeignError >>> NonEmptyList.singleton) >>> except)

instance writeForeignOrganizationNW :: WriteForeign OrganizationNW where
    writeImpl organization = writeImpl $ toVariantNW organization

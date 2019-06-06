{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- TODO
module Data.Aeson.EnumLike
  ( EnumLike(..)
  ) where

import           Prelude

import           Control.Applicative ((<|>))
import qualified Data.Aeson          as Aeson
import           qualified Data.Aeson.Types    as Aeson (Parser)
import           Data.Kind           (Type)
import           Data.Proxy          (Proxy(..))
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Typeable       (Typeable, typeOf)
import           GHC.Generics
import           GHC.TypeLits        (KnownSymbol, symbolVal)

-- |
-- TODO
newtype EnumLike a = EnumLike { getEnumLike :: a }

instance (Typeable a, Generic a, FromText (Rep a)) => Aeson.FromJSON (EnumLike a) where
  parseJSON =
    Aeson.withText (typeName @a) $ \txt -> do
      a <- fromText @(Rep a) txt <|> helpfulFailure txt
      pure . EnumLike . to $ a
    where
    helpfulFailure :: Text -> Aeson.Parser b
    helpfulFailure txt =
      fail $ "unknown value for " <> typeName @a <> ": " <> show txt

instance (Generic a, ToText (Rep a)) => Aeson.ToJSON (EnumLike a) where
  toJSON (EnumLike a) = Aeson.String $ toText (from a)


class FromText (f :: Type -> Type) where
  fromText :: Text -> Aeson.Parser (f p)

instance FromText f => FromText (M1 i c f) where
  fromText txt = M1 <$> fromText @f txt

instance (FromText f, FromText g) => FromText (f :+: g) where
  fromText txt = (L1 <$> fromText @f txt) <|> (R1 <$> fromText @g txt)

instance (KnownSymbol key) => FromText (Rec0 (Proxy key)) where
  fromText txt
    | txt == Text.pack (symbolVal (Proxy @key)) = pure (K1 Proxy)
    | otherwise = fail "nope"

instance FromText U1 where
  fromText "" = pure U1
  fromText other = fail ("expecting empty text, got: " <> show other)


class ToText (f :: Type -> Type) where
  toText :: f p -> Text

instance ToText f => ToText (M1 i c f) where
  toText (M1 f) = toText f

instance (ToText f, ToText g) => ToText (f :+: g) where
  toText (L1 f) = toText f
  toText (R1 g) = toText g

instance (KnownSymbol key) => ToText (Rec0 (Proxy key)) where
  toText _ = Text.pack (symbolVal (Proxy @key))

instance ToText U1 where
  toText U1 = ""


typeName :: forall a. Typeable a => String
typeName = show (typeOf (undefined :: a))

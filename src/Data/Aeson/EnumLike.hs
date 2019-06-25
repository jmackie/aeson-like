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
-- Decorate sum types with their stringy representations.
--
-- Note this module is only useful with @-XDerivingVia@ (introduced in GHC 8.6.x).
--
module Data.Aeson.EnumLike
  ( EnumLike(..)
  ) where

import           Prelude

import           Control.Applicative ((<|>))
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.Types    as Aeson (Parser)
import           Data.Kind           (Type)
import           Data.Proxy          (Proxy(..))
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Typeable       (Typeable, typeOf)
import           GHC.Generics
import           GHC.TypeLits

-- $setup
-- >>> :set -XDeriveGeneric
-- >>> :set -XDataKinds
-- >>> import GHC.Generics (Generic)
-- >>> import qualified Data.Aeson as Aeson

-- |
-- A @DerivingVia@ helper for generating 'Aeson.FromJSON' and 'Aeson.ToJSON'
-- instances. 'EnumLike' @a@ has these instances when @a@ is a sum of 'Symbol'
-- proxies.
--
-- === Example
--
-- >>> :set -XDerivingVia
-- >>> import Data.Proxy (Proxy(..))
-- >>> :{
-- data Reaction
--   = ThumbsUp   (Proxy "+1")
--   | ThumbsDown (Proxy "-1")
--   | Laugh      (Proxy "laugh")
--   | Confused   (Proxy "confused")
--   | Heart      (Proxy "heart")
--   | Hooray     (Proxy "hooray")
--   | Rocket     (Proxy "rocket")
--   | Eyes       (Proxy "eyes")
--   deriving (Generic, Show)
--   deriving (Aeson.ToJSON, Aeson.FromJSON) via (EnumLike Reaction)
-- :}
--
-- (<https://developer.github.com/v3/reactions/#reaction-types>)
--
-- >>> Aeson.decode @Reaction "\"heart\""
-- Just (Heart Proxy)
--
-- The redundant 'Proxy' associated with each constructor is a /bit/ annoying,
-- but empty record patterns can help here:
--
-- >>> :{
-- isLaugh Laugh{} = True
-- isLaugh _       = False
-- :}
--
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
  fromText ""    = pure U1
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

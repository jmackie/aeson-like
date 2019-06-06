{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- TODO
--
module Data.Aeson.ObjectLike
  ( ObjectLike(..)
  , Prop(..)
  , reprop
  ) where

import           Prelude

import           Control.Applicative (liftA2)
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.Types    as Aeson (Parser)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HashMap
import           Data.Kind           (Type)
import           Data.Proxy          (Proxy(..))
import qualified Data.Text           as Text
import           Data.Typeable       (Typeable, typeOf)
import           GHC.Generics
import           GHC.TypeLits        (KnownSymbol, Symbol, symbolVal)

-- |
-- @ObjectLike@ is our deriving via helper.
--
-- If a data type is equivalent to a bunch of @Prop@s then it has an instance
-- of @FromObject@.
--
-- If a data type is equivalent to a bunch of @Prop@s then it has an instance
-- of @ToObject@.
--
-- You shouldn't use the constructor, it's just for DerivingVia
newtype ObjectLike a = ObjectLike { getObjectLike :: a }

instance (Typeable a, Generic a, FromObject (Rep a)) => Aeson.FromJSON (ObjectLike a) where
  parseJSON =
    Aeson.withObject (typeName @a) $ \obj -> do
      a <- fromObject @(Rep a) obj
      pure . ObjectLike . to $ a

instance (Generic a, ToObject (Rep a)) => Aeson.ToJSON (ObjectLike a) where
  toJSON (ObjectLike a) = Aeson.Object $ toObject (from a)


-- |
-- @Prop@ lets us capture the keys associated with parts of a product type.
newtype Prop (key :: Symbol) a = Prop { unProp :: a }
  deriving stock (Generic, Functor, Foldable, Traversable)
  deriving newtype (Eq, Ord, Show, Aeson.ToJSON, Aeson.FromJSON, Hashable)

-- |
-- TODO
reprop :: forall key key' a. Prop key' a -> Prop key a
reprop (Prop a) = Prop a


class FromObject (f :: Type -> Type) where
  fromObject :: Aeson.Object -> Aeson.Parser (f p)

instance FromObject f => FromObject (M1 i c f) where
  fromObject obj = M1 <$> fromObject @f obj

instance (FromObject f, FromObject g) => FromObject (f :*: g) where
  fromObject obj = liftA2 (:*:) (fromObject obj) (fromObject obj)

instance (KnownSymbol key, Aeson.FromJSON a) => FromObject (Rec0 (Prop key (Maybe a))) where
  fromObject obj = K1 . Prop <$> obj Aeson..:? key
    where key = Text.pack $ symbolVal (Proxy @key)

instance {-# iNCoHErEnt #-} (KnownSymbol key, Aeson.FromJSON a) => FromObject (Rec0 (Prop key a)) where
  fromObject obj = K1 . Prop <$> obj Aeson..: key
    where key = Text.pack $ symbolVal (Proxy @key)

instance FromObject U1 where
  fromObject obj 
    | HashMap.null obj = pure U1
    | otherwise = fail "expecting an empty object" 


class ToObject (f :: Type -> Type) where
  toObject :: f p -> Aeson.Object

instance ToObject f => ToObject (M1 i c f) where
  toObject (M1 f) = toObject f

instance (ToObject f, ToObject g) => ToObject (f :*: g) where
  toObject (f :*: g) = toObject f <> toObject g

instance (KnownSymbol key, Aeson.ToJSON a) => ToObject (Rec0 (Prop key a)) where
  toObject (K1 (Prop a)) = HashMap.singleton key (Aeson.toJSON a)
    where key = Text.pack $ symbolVal (Proxy @key)

instance ToObject U1 where
  toObject U1 = HashMap.empty

typeName :: forall a. Typeable a => String
typeName = show (typeOf (undefined :: a))

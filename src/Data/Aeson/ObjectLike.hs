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
-- Decorate product/record types with JSON object keys.
--
-- This module is only useful with '-XDerivingVia' (introduced in GHC 8.6.x).
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

-- $setup
-- >>> :set -XDeriveGeneric
-- >>> :set -XDataKinds
-- >>> import GHC.Generics (Generic)
-- >>> import qualified Data.Aeson as Aeson

-- |
-- A @DerivingVia@ helper for generating 'Aeson.FromJSON' and 'Aeson.ToJSON'
-- instances. 'ObjectLike' @a@ has these instances when @a@ is essentially a 
-- bunch of 'Prop' values.
--
-- === Example
--
-- >>> :set -XDerivingVia
-- >>> :{
-- data PowerStats 
--   = PowerStats
--       { strengthStat :: Prop "strength" String
--       , speedStat    :: Prop "speed"    String
--       , powerStat    :: Prop "power"    String
--       , combatStat   :: Prop "combat"   String
--       }
--   deriving (Generic, Show)
--   deriving (Aeson.ToJSON, Aeson.FromJSON) via (ObjectLike PowerStats)
-- :}
--
-- (<https://superheroapi.com/#powerstats>)
--
-- >>> let stats = PowerStats (Prop "26") (Prop "27") (Prop "47") (Prop "100")
-- >>> let json = Aeson.encode stats
-- >>> json
-- "{\"strength\":\"26\",\"combat\":\"100\",\"speed\":\"27\",\"power\":\"47\"}"
-- >>> Aeson.decode @PowerStats json
-- Just (PowerStats {strengthStat = "26", speedStat = "27", powerStat = "47", combatStat = "100"})
--
newtype ObjectLike a = ObjectLike { getObjectLike :: a }

instance (Typeable a, Generic a, FromObject (Rep a)) => Aeson.FromJSON (ObjectLike a) where
  parseJSON =
    Aeson.withObject (typeName @a) $ \obj -> do
      a <- fromObject @(Rep a) obj
      pure . ObjectLike . to $ a

instance (Generic a, ToObject (Rep a)) => Aeson.ToJSON (ObjectLike a) where
  toJSON (ObjectLike a) = Aeson.Object $ toObject (from a)

-- |
-- 'Prop' (short for property) associates some value with its key in a JSON
-- object. Hence it's only really useful as a field of some product type (see
-- the example for 'ObjectLike'.
--
-- Note that type applying the constructor can introduce an extra sanity check, 
-- much like bare record selectors.
--
-- >>> :set -XTypeApplications
-- >>> data Point = Point (Prop "x" Int) (Prop "y" Int) deriving (Generic, Show)
-- >>> Point (Prop @"x" 24) (Prop @"y" 42)
-- Point 24 42
newtype Prop (key :: Symbol) a = Prop { unProp :: a }
  deriving stock (Generic, Functor, Foldable, Traversable)
  deriving newtype (Eq, Ord, Show, Aeson.ToJSON, Aeson.FromJSON, Hashable)

-- |
-- Coerce the key associated with a 'Prop' value.
reprop :: forall oldkey newkey a. Prop oldkey a -> Prop newkey a
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

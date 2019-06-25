{-# LANGUAGE DerivingStrategies #-}
-- | 
-- Interpret some JSON without losing the original data. 
--
module Data.Aeson.SomethingLike 
  ( SomethingLike
  , unwrap
  ) where

import           Prelude

import qualified Data.Aeson as Aeson

-- $setup
-- >>> :set -XDeriveGeneric
-- >>> :set -XDeriveAnyClass
-- >>> :set -XTypeApplications
-- >>> :set -XOverloadedStrings
-- >>> :set -XFlexibleContexts
-- >>> import GHC.Generics (Generic)
-- >>> import qualified Data.Aeson as Aeson
-- >>> import Data.Function ((&))
-- >>> import Data.Maybe (fromJust)

-- | 
-- A wrapper around some value that can only be constructed via 'Aeson.parseJSON',
-- and holds on to the original 'Aeson.Value'.
--
-- The 'Aeson.ToJSON' instance will simply return the captured 'Aeson.Value'.
--
-- === Example
-- 
-- Suppose we have the following type that we expect receive from an API we
-- don't fully understand...
--
-- >>> :{
-- data Planet 
--   = Planet 
--       { name    :: String 
--       , gravity :: String
--       } 
--   deriving (Show, Generic, Aeson.FromJSON)
-- :}
--
-- (<https://swapi.co/documentation#planets>)
--
-- In reality we expect the API to give us more data than this, and we don't 
-- want to drop that data on the floor, so we decode into a 'SomethingLike' @Planet@
-- 
-- >>> let json = "{\"name\":\"Tatooine\",\"climate\":\"Arid\",\"gravity\":\"1\"}"
-- >>> let planet = Aeson.decode @(SomethingLike Planet) json & fromJust
--
-- We can then 'unwrap' the part of that data we understand:
--
-- >>> print (unwrap planet)
-- Planet {name = "Tatooine", gravity = "1"}
--
-- And we can serialize the original json (e.g. to a log):
--
-- >>> print (Aeson.encode planet)
-- "{\"name\":\"Tatooine\",\"climate\":\"Arid\",\"gravity\":\"1\"}"
-- >>> Aeson.encode planet == json
-- True
-- 
data SomethingLike a = SomethingLike Aeson.Value a
  deriving stock (Show)

instance Aeson.FromJSON a => Aeson.FromJSON (SomethingLike a) where
  parseJSON value = SomethingLike value <$> Aeson.parseJSON value

instance Aeson.ToJSON (SomethingLike a) where
  toJSON (SomethingLike value _) = value

-- | 
-- Return the "interpreted" value.
unwrap :: SomethingLike a -> a
unwrap (SomethingLike _ a) = a

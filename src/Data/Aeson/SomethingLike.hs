{-# LANGUAGE DerivingStrategies #-}
-- | 
-- TODO: Document dis
module Data.Aeson.SomethingLike 
  ( SomethingLike
  , unwrap
  ) where

import           Prelude

import qualified Data.Aeson as Aeson

data SomethingLike a = SomethingLike Aeson.Value a
  deriving stock (Show)

instance Aeson.FromJSON a => Aeson.FromJSON (SomethingLike a) where
  parseJSON value = SomethingLike value <$> Aeson.parseJSON value

instance Aeson.ToJSON (SomethingLike a) where
  toJSON (SomethingLike value _) = value

unwrap :: SomethingLike a -> a
unwrap (SomethingLike _ a) = a

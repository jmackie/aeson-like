{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}
module Data.Aeson.Interpreted 
  ( Interpreted
  , unwrap
  ) where

import           Prelude

import qualified Data.Aeson as Aeson

data Interpreted a = Interpreted Aeson.Value a
  deriving stock (Functor, Foldable, Traversable)

instance Aeson.FromJSON a => Aeson.FromJSON (Interpreted a) where
  parseJSON value = Interpreted value <$> Aeson.parseJSON value

instance Aeson.ToJSON (Interpreted a) where
  toJSON (Interpreted value _) = value

unwrap :: Interpreted a -> a
unwrap (Interpreted _ a) = a

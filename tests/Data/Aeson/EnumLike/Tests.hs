{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Aeson.EnumLike.Tests
  ( tests
  ) where

import           Prelude

import qualified Data.Aeson          as Aeson
import qualified Data.List           as List
import           Data.Proxy          (Proxy(..))
import           GHC.Generics        (Generic)
import qualified Test.Tasty          as Tasty
import           Test.Tasty.HUnit    ((@=?))
import qualified Test.Tasty.HUnit    as Tasty.HUnit

import           Data.Aeson.EnumLike (EnumLike(..))

tests :: Tasty.TestTree
tests = Tasty.testGroup "EnumLike" [ unitTests ]

unitTests :: Tasty.TestTree
unitTests = Tasty.testGroup "unit tests"
  [ Tasty.HUnit.testCase "encodes as expected" $ do
      Aeson.encode (Foo Proxy) @=? "\"foo\""
      Aeson.encode (Bar Proxy) @=? "\"bar\""

  , Tasty.HUnit.testCase "decodes as expected" $ do
      case Aeson.decode "\"foo\"" of
        Nothing      -> Tasty.HUnit.assertFailure ""
        Just decoded -> decoded @=? Foo Proxy

      case Aeson.decode "\"bar\"" of
        Nothing      -> Tasty.HUnit.assertFailure ""
        Just decoded -> decoded @=? Bar Proxy

  , Tasty.HUnit.testCase "decoding errors are helpful" $
      case Aeson.eitherDecode "\"nope\"" of
        Right (_ :: FooBar) -> Tasty.HUnit.assertFailure "decoding should have failed"
        Left err -> do
          Tasty.HUnit.assertBool "error contains the type name" (err `contains` "FooBar")
          Tasty.HUnit.assertBool "error contains the bad values" (err `contains` "\"nope\"")
  ]

data FooBar
  = Foo (Proxy "foo")
  | Bar (Proxy "bar")
  deriving (Generic, Eq, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (EnumLike FooBar)

contains :: String -> String -> Bool
contains = flip List.isInfixOf

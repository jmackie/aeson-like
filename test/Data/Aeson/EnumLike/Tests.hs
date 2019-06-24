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
  [ Tasty.HUnit.testCase "FooBar encodes as expected" $ do
      Aeson.encode (Foo Proxy) @=? "\"foo\""
      Aeson.encode (Bar Proxy) @=? "\"bar\""

  , Tasty.HUnit.testCase "FooBar decodes as expected" $ do
      case Aeson.eitherDecode "\"foo\"" of
        Left err -> Tasty.HUnit.assertFailure ("decoding failed: " <> err)
        Right decoded -> decoded @=? Foo Proxy

      case Aeson.eitherDecode "\"bar\"" of
        Left err -> Tasty.HUnit.assertFailure ("decoding failed: " <> err)
        Right decoded -> decoded @=? Bar Proxy

  , Tasty.HUnit.testCase "Stub encodes as expected" $
      Aeson.encode Stub @=? "\"\""

  , Tasty.HUnit.testCase "Stub decodes as expected" $
      case Aeson.eitherDecode "\"\"" of
        Left err -> Tasty.HUnit.assertFailure ("decoding failed: " <> err)
        Right decoded -> decoded @=? Stub

  , Tasty.HUnit.testCase "decoding errors are helpful" $
      case Aeson.eitherDecode "\"nope\"" of
        Right (_ :: FooBar) -> Tasty.HUnit.assertFailure "decoding should have failed"
        Left err -> do
          Tasty.HUnit.assertBool "error contains the type name" 
            (err `contains` "FooBar")

          Tasty.HUnit.assertBool "error contains the bad values" 
            (err `contains` "\"nope\"")
  ]

data FooBar
  = Foo (Proxy "foo")
  | Bar (Proxy "bar")
  deriving (Generic, Eq, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (EnumLike FooBar)

data Stub = Stub 
  deriving (Generic, Eq, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (EnumLike Stub)

contains :: String -> String -> Bool
contains = flip List.isInfixOf

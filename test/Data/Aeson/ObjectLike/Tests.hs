{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Data.Aeson.ObjectLike.Tests
  ( tests
  ) where

import           Prelude

import qualified Data.Aeson            as Aeson
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.List           as List
import           GHC.Generics          (Generic)
import qualified Test.Tasty            as Tasty
import           Test.Tasty.HUnit      ((@=?))
import qualified Test.Tasty.HUnit      as Tasty.HUnit

import           Data.Aeson.ObjectLike (ObjectLike(..), Prop(..))

tests :: Tasty.TestTree
tests = Tasty.testGroup "ObjectLike" [ unitTests ]

unitTests :: Tasty.TestTree
unitTests = Tasty.testGroup "unit tests"

  [ Tasty.HUnit.testCase "User encodes as expected" $
      Aeson.encode exampleUser @=? exampleUserEncoded

  , Tasty.HUnit.testCase "User decodes as expected" $
      case Aeson.eitherDecode exampleUserEncoded of
        Left err -> Tasty.HUnit.assertFailure ("decoding failed: " <> err)
        Right decoded -> decoded @=? exampleUser

  , Tasty.HUnit.testCase "Token (newtype) encodes as expected" $
      Aeson.encode exampleToken @=? exampleTokenEncoded

  , Tasty.HUnit.testCase "Token (newtype) decodes as expected" $
      case Aeson.eitherDecode exampleTokenEncoded of
        Left err -> Tasty.HUnit.assertFailure ("decoding failed: " <> err)
        Right decoded -> decoded @=? exampleToken

  , Tasty.HUnit.testCase "Patchy decodes from an empty object" $
      case Aeson.eitherDecode "{}" of
        Left err -> Tasty.HUnit.assertFailure ("decoding failed: " <> err)
        Right decoded -> 
          decoded @=? Patchy (Prop @"foo" Nothing) (Prop @"bar" Nothing)

  , Tasty.HUnit.testCase "Patchy decodes from a partial object" $
      case Aeson.eitherDecode "{\"foo\":42}" of
        Left err -> Tasty.HUnit.assertFailure ("decoding failed: " <> err)
        Right decoded -> 
          decoded @=? Patchy (Prop @"foo" (Just 42)) (Prop @"bar" Nothing)

  , Tasty.HUnit.testCase "empty Patchy encodes to nulls" $
      Aeson.encode (Patchy (Prop @"foo" Nothing) (Prop @"bar" Nothing)) 
        @=? "{\"foo\":null,\"bar\":null}"

  , Tasty.HUnit.testCase "Units decodes as expected" $
      -- NOTE: aeson represents () as an empty array
      case Aeson.eitherDecode "{\"unit\":[],\"another_unit\":[]}" of
        Left err -> Tasty.HUnit.assertFailure ("decoding failed: " <> err)
        Right decoded -> 
          decoded @=? Units (Prop @"unit" ()) (Prop @"another_unit" ())

  , Tasty.HUnit.testCase "Units encodes as expected" $
      Aeson.encode (Units (Prop @"unit" ()) (Prop @"another_unit" ()))
        @=? "{\"another_unit\":[],\"unit\":[]}"

  , Tasty.HUnit.testCase "Stub decodes as expected" $
      case Aeson.eitherDecode "{}" of
        Left err -> Tasty.HUnit.assertFailure ("decoding failed: " <> err)
        Right decoded -> decoded @=? Stub

  , Tasty.HUnit.testCase "Stub encodes as expected" $
      Aeson.encode Stub @=? "{}"

  , Tasty.HUnit.testCase "decoding errors are helpful" $
      case Aeson.eitherDecode "{\"names\":\"Foo Bar\",\"id\":1}" of
        Right (_ :: User) -> Tasty.HUnit.assertFailure "decoding should have failed"
        Left err ->
          Tasty.HUnit.assertBool "error mentions the missing key"
            (err `contains` "key \"name\" not present")
  ]

data User = User
  { userId   :: Prop "id" Int
  , userName :: Prop "name" String
  }
  deriving (Generic, Eq, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ObjectLike User)

exampleUser :: User
exampleUser =
  User (Prop @"id" 1) (Prop @"name" "Foo Bar")   -- I think this is neat...
--User { userId = Prop 1, userName = Prop "Foo Bar" }

exampleUserEncoded :: LBS.ByteString
exampleUserEncoded = "{\"name\":\"Foo Bar\",\"id\":1}"

newtype Token = Token (Prop "token" String)
  deriving stock (Generic, Eq, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ObjectLike Token)

exampleToken :: Token
exampleToken = Token (Prop "abcde")

exampleTokenEncoded :: LBS.ByteString
exampleTokenEncoded = "{\"token\":\"abcde\"}"

data Patchy = Patchy
  { patchyInt    :: Prop "foo" (Maybe Int)
  , patchyString :: Prop "bar" (Maybe String)
  }
  deriving (Generic, Eq, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ObjectLike Patchy)

data Units = Units
  { why :: Prop "unit" ()
  , tho :: Prop "another_unit" ()
  }
  deriving (Generic, Eq, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ObjectLike Units)

data Stub = Stub 
  deriving (Generic, Eq, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ObjectLike Stub)

contains :: String -> String -> Bool
contains = flip List.isInfixOf

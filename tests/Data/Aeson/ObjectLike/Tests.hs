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
  [ Tasty.HUnit.testCase "encodes as expected" $ 
      Aeson.encode exampleUser @=? exampleUserEncoded

  , Tasty.HUnit.testCase "decodes as expected" $
      case Aeson.decode exampleUserEncoded of
        Nothing -> Tasty.HUnit.assertFailure "example should have decoded"
        Just decoded -> decoded @=? exampleUser

  , Tasty.HUnit.testCase "decoding errors are helpful" $
      case Aeson.eitherDecode "{\"names\":\"Foo Bar\",\"id\":1}" of
        Right (_ :: User) -> Tasty.HUnit.assertFailure "decoding should have failed"
        Left err -> 
          Tasty.HUnit.assertBool "error mentions the missing key" (err `contains` "key \"name\" not present")
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

contains :: String -> String -> Bool
contains = flip List.isInfixOf

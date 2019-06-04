module Main
  ( main
  ) where

import           Prelude

import qualified Test.Tasty                  as Tasty

import qualified Data.Aeson.EnumLike.Tests   as EnumLike
import qualified Data.Aeson.ObjectLike.Tests as ObjectLike

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "Tests"
  [ ObjectLike.tests
  , EnumLike.tests
  ]

# `aeson-like`

[![Build Status](https://travis-ci.org/jmackie/aeson-like.svg?branch=master)](https://travis-ci.org/jmackie/aeson-like)

> NOTE: name is subject to change cos I don't like it.

## tl;dr

A bunch of utilities to make developing against `json` APIs easier.

<details>
<summary>Code Preamble</summary>
<p>

All the code in this file is compiled and tested, so we need a module header.

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import Prelude

--import GHC.Generics (Generic)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
--import Data.Proxy (Proxy(..))
import Test.Tasty
import Test.Tasty.HUnit

--import Data.Aeson.ObjectLike (ObjectLike(..), Prop(..))
--import Data.Aeson.EnumLike (EnumLike(..))
```

</p>
</details>

## `Data.Aeson.ObjectLike`

Suppose we have the following (_obligatory_) user type:

```json
{
  "id": 42,
  "name": "foo",
  "age": 24
}
```

If we want to decode this to a haskell type we have a few choices.

**Option 1**: Crack open the [`aeson`][aeson-hackage] docs and remember how to
write instances by hand

```haskell

data User = User
  { userId   :: Int
  , userName :: String
  , userAge  :: Int
  }
  deriving (Show, Eq)

instance Aeson.FromJSON User where
  parseJSON =
    Aeson.withObject "User" $ \object ->
      User <$> object .: "id"
           <*> object .: "name"
           <*> object .: "age"

instance Aeson.ToJSON User where
  toJSON user =
    Aeson.object
      [ "id"   .= userId user
      , "name" .= userName user
      , "age"  .= userAge user
      ]
```

That's nice and explicit, but for any non-trivial application you run into
several issues:

1. It's noisy. The important logic - i.e. which record fields map to which
   object keys - can get lost in the boilerplate.

2. It's bloated. That boilerplate can make for unwieldy modules quite
   quickly. While _explicit_ code like this is generally easier to read and
   follow, I find that's true only up until a certain amount of code.

3. The logic for mapping record selectors to object keys is duplicated.

4. If you're writing a lot of mechanical code like this, it's very easy to get
   wrong. And the compiler mostly isn't going to help you.

## TODO

- [ ] Write this README
- [ ] Better type errors
- [ ] Document things
- [ ] More tests (including hedgehog properties)
- [x] Travis CI

<details>
<summary>Tests</summary>
<p>

```haskell
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "README"
  [ testCase "User example" $ do
      Aeson.encode (User 1 "john doe" 42) @?=
        "{\"age\":42,\"name\":\"john doe\",\"id\":1}"

      Aeson.decode "{\"age\":42,\"name\":\"john doe\",\"id\":1}" @?=
        Just (User 1 "john doe" 42)
  ]
```

</p>
</details>

[aeson-hackage]: https://hackage.haskell.org/package/aeson

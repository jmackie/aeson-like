# `aeson-like`

> Travis badge ere

<details>
<summary>Code Preamble</summary>
<p>
All the code in this file is compiled and tested, so we need a module header
and stuff...

```haskell
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DerivingVia      #-}
{-# LANGUAGE TypeApplications #-}

import Prelude
import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import Data.Proxy (Proxy(..))
import Test.Tasty
import Test.Tasty.HUnit
```

</p>
</details>

## tl;dr 

Easy `ToJSON` and `FromJSON` instances for types modelling json data

 - Put object keys at the type level and keep record selector names idiomatic.
 - Map sum types to strings at the type level and avoid writing loads of boilerplate.


```haskell
import Data.Aeson.ObjectLike (ObjectLike(..), Prop(..))
import Data.Aeson.EnumLike (EnumLike(..))

-- |
-- Encodes/decodes as:
-- 
-- { "id": 42, "name": "foo", "dog_preference": "all_dogs" }
--
data User = User
  { userId :: Prop "id" Int
  , userName :: Prop "name" String
  , userDogPreference :: Prop "dog_preference" DogPreference
  }
  deriving (Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (ObjectLike User)


data DogPreference
  = AllDogs (Proxy "all_dogs")
  | AlmostAllDogs (Proxy "almost_all_dogs")
  deriving (Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (EnumLike DogPreference)

```

## Motivation

If you've ever developed against a JSON API in haskell 

## TODO

- [ ] Write this README
- [ ] Better type errors
- [ ] Document things
- [ ] More tests (including hedgehog properties)
- [ ] Travis CI

<details>
<summary>Tests</summary>
<p>

```haskell
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "README"
  [ testCase "User encodes like I said it does" $ do
      let user = User (Prop @"id" 42) 
                      (Prop @"name" "foo") 
                      (Prop @"dog_preference" (AllDogs Proxy))

      Aeson.encode user @?= 
        "{\"dog_preference\":\"all_dogs\",\"name\":\"foo\",\"id\":42}"
  ]
```

</p>
</details>

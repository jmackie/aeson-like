# `aeson-like`

[![Build Status](https://travis-ci.org/jmackie/aeson-like.svg?branch=master)](https://travis-ci.org/jmackie/aeson-like)

A collection of things I've found useful when developing against
third-party/vendor/non-haskell JSON APIs.

To demonstrate how this can be useful we'll work through a real-world example:
consuming the [PokéAPI](https://pokeapi.co/), cos Pokémon is life. Specifically we want to be able to
query information about a Pokémon by name.

But first, the obligatory preamble:

```haskell
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DerivingVia      #-}
{-# LANGUAGE TypeApplications #-}

import GHC.Generics         (Generic)
import Data.Proxy           (Proxy(..))
import Data.Text            (Text)
import Data.ByteString.Lazy (ByteString)

import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client

import qualified Data.Aeson as Aeson
```

And here are the imports that we're interested in:

```haskell
import Data.Aeson.ObjectLike    (ObjectLike(..), Prop(..))
import Data.Aeson.EnumLike      (EnumLike(..))
import Data.Aeson.SomethingLike (SomethingLike)
```

## `Data.Aeson.ObjectLike`

We need to model the `Pokémon` object returned by the API.

To get a working `FromJSON` instance you would typically do one of the following:

1. Write the instance by hand.
2. Align your record selector names with the keys of the expected object, so
   the default generic instance Just Works.
3. Pass some custom options to [`genericParseJSON`](https://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:genericParseJSON) 
   to map the expected object keys to nice haskell record selectors.

But for any non-trivial project these can all become annoying. Having lots of hand-written
instances is noisy and unwieldy. Using object keys (e.g. `id`, `type`...) as
record selectors quickly leads to name clashes and the dreaded
`-XDuplicateRecordFields`. And fiddling with `genericParseJSON` options can
be obscure and hard to debug.

I would rather have the logic for mapping selectors to object keys defined 
alongside the type. Introducing `ObjectLike`...

```haskell
data Pokemon
  = Pokemon
      { pokemonName  :: Prop "name" Text
      , pokemonId    :: Prop "id" Int
      , pokemonTypes :: Prop "types" [PokemonType]
      }

  deriving (Generic, Show)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (ObjectLike Pokemon)
```

`ObjectLike a` has `ToJSON` and `FromJSON` instances if `a` is essentially
a product of `Prop` types. 

`Prop` is a newtype that carries a type-level string, where that string 
corresponds to a key in the expected JSON object.

```haskell
data PokemonType
  = PokemonType (Prop "slot" Int) (Prop "type" (NamedAPIResource Type))

  deriving (Generic, Show)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (ObjectLike PokemonType)

data NamedAPIResource a
  = NamedAPIResource
      { resourceName :: Prop "name" a
      , resourceUrl  :: Prop "url" Text
      }

  deriving (Generic, Show)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (ObjectLike (NamedAPIResource a))
```

## `Data.Aeson.EnumLike`

Often when working with JSON APIs we'll want to decode a string into a nice sum 
type. The issue here is that the logic for mapping strings to constructors is 
split across two separate `ToJSON` and `FromJSON` instances. And writing those 
instances is mechanical and tedious.

We can instead associate each constructor with a type-level string and use that
informating to generically derive `ToJSON` and `FromJSON` instances.

```haskell
data Type
  = FireType (Proxy "fire")
  | PsychicType (Proxy "psychic")
  | GroundType (Proxy "ground")
  -- ...etc

  deriving (Generic, Show)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (EnumLike Type)
```

## `Data.Aeson.SomethingLike`

If you're only decoding part of the expected data (intentionally or unintentionally) 
you might want to keep the original data around. 

```haskell
decodePokemonResponse 
  :: Client.Response ByteString -> Either String (SomethingLike Pokemon)
decodePokemonResponse = Aeson.eitherDecode . Client.responseBody
```

## Putting it all together

```haskell
main :: IO ()
main = do
  manager  <- Client.newManager Client.tlsManagerSettings
  request  <- Client.parseRequest "GET https://pokeapi.co/api/v2/pokemon/ditto"
  response <- Client.httpLbs request manager
  print $ decodePokemonResponse response
```

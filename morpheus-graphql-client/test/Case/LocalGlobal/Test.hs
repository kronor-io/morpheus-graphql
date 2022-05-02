{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.LocalGlobal.Test
  ( test,
  )
where

import Case.LocalGlobal.Api
import Data.Aeson
import Data.Eq (Eq)
import Data.Morpheus.Client
  ( Fetch (..),
    ID,
    declareGlobalTypes,
    declareLocalTypes,
    declareLocalTypesInline,
    raw,
  )
import Data.Semigroup ((<>))
import Data.Text (Text)
import Spec.Utils
  ( getFile,
  )
import Test.Tasty
  ( TestTree,
  )
import Test.Tasty.HUnit
  ( assertEqual,
    testCase,
  )
import Prelude
  ( Either (..),
    FilePath,
    IO,
    Maybe (Just, Nothing),
    Show (show),
    ($),
    (>>=),
  )

declareGlobalTypes schema

declareLocalTypesInline
  schema
  [raw|
    query GetCities ( $inputCity: City!) {
      city(city:$inputCity)
      cities
    }
  |]

declareLocalTypes schema (loc "users1.gql")
declareLocalTypes schema (loc "users2.gql")

checkQuery ::
  ( Fetch a,
    FromJSON a,
    Eq a,
    Show a
  ) =>
  FilePath ->
  Args a ->
  a ->
  IO ()
checkQuery p args v =
  fetch
    (\_ -> getFile ("LocalGlobal/" <> p <> ".json"))
    args
    >>= assertEqual ("Test " <> show p) (Right v)

checkCities :: IO ()
checkCities =
  checkQuery
    "cities"
    GetCitiesArgs {inputCity = CityAthens}
    GetCities
      { city = CityAthens,
        cities =
          [ CityAthens,
            CitySparta,
            CityCorinth,
            CityDelphi
          ]
      }

checkUsers1 :: IO ()
checkUsers1 =
  checkQuery
    "users1"
    GetUsers1Args {user = UserInput {name = "odysseus"}}
    GetUsers1
      { user =
          Just
            ( GetUsers1UserUser
                { name = "Odysseus",
                  home = Just CityIthaca
                }
            )
      }

checkUsers2 :: IO ()
checkUsers2 =
  checkQuery
    "users2"
    GetUsers2Args {user = UserInput {name = "odysseus"}}
    GetUsers2
      { user =
          Just
            ( GetUsers2UserUser
                { name = "Morpheus"
                }
            )
      }

test :: TestTree
test = testCase "Test Local/Global types" $ do
  checkCities
  checkUsers1
  checkUsers2
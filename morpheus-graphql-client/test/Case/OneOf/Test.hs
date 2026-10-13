{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.OneOf.Test
  ( test,
  )
where

import qualified Case.OneOf.Introspection as Introspection
import Data.Aeson (Value, decode, toJSON)
import Data.Morpheus.Client
  ( declareGlobalTypes,
    declareLocalTypesInline,
    raw,
  )
import Relude
import Spec.Utils
  ( assertFetch,
    path,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertEqual,
    testCase,
  )

declareGlobalTypes (path "OneOf/schema.gql")

declareLocalTypesInline
  (path "OneOf/schema.gql")
  [raw|
    query Pay($method: PaymentMethod!) {
      pay(method: $method)
    }
  |]

expectJSON :: LByteString -> Maybe Value
expectJSON = decode

test :: TestTree
test =
  testGroup
    "OneOf"
    [ assertFetch
        "OneOf"
        Nothing
        PayArgs {method = PaymentMethodSwish "0701234567"}
        (Right Pay {pay = "paid"}),
      testCase "encode" $ do
        assertEqual "card" (expectJSON "{\"card\":{\"number\":\"4111\"}}") (Just $ toJSON $ PaymentMethodCard CardInput {number = "4111"})
        assertEqual "swish" (expectJSON "{\"swish\":\"0701234567\"}") (Just $ toJSON $ PaymentMethodSwish "0701234567")
        assertEqual "ids" (expectJSON "{\"ids\":[1,2]}") (Just $ toJSON $ PaymentMethodIds [1, 2])
        assertEqual "args" (expectJSON "{\"method\":{\"ids\":[]}}") (Just $ toJSON PayArgs {method = PaymentMethodIds []}),
      testCase "introspection"
        $ assertEqual "swish" (expectJSON "{\"swish\":\"x\"}") (Just $ toJSON $ Introspection.PaymentMethodSwish "x")
    ]

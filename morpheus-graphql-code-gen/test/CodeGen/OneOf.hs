{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CodeGen.OneOf
  ( tests,
  )
where

import CLI.Config
  ( Config (..),
    Service (..),
    ServiceOptions (..),
    readConfig,
  )
import CLI.Generator
  ( BuildConfig (..),
    processServerDocument,
  )
import qualified CodeGen.OneOfTH as TH
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.List (isInfixOf)
import Data.Morpheus.Internal.Ext (Result (..))
import Relude
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )

fixtureDir :: FilePath
fixtureDir = "test/fixtures/oneof"

schemaPath :: FilePath
schemaPath = fixtureDir </> "schema.graphql"

configPath :: FilePath
configPath = fixtureDir

tests :: TestTree
tests =
  testGroup
    "oneOf"
    [ testCase "template-haskell path generates sum constructors" testTemplateHaskellCodeGen,
      testCase "yaml path generates sum constructors" testYamlCodeGen
    ]

loadSchema :: IO L.ByteString
loadSchema = L.readFile schemaPath

testTemplateHaskellCodeGen :: Assertion
testTemplateHaskellCodeGen =
  assertEqual
    "constructor names"
    ["UserById", "UserByEmail"]
    TH.constructorNames

testYamlCodeGen :: Assertion
testYamlCodeGen = do
  Config {server} <- readConfig configPath
  services <- requireJust "missing server config" server
  Service {source, options} <-
    requireJust "missing service definition" (listToMaybe services)
  schema <- loadSchema
  generated <-
    case processServerDocument BuildConfig {root = source, buildOptions = fromMaybe (ServiceOptions False [] mempty) options} "Generated.OneOf" schema of
      Failure errors -> assertFailure (show errors)
      Success {result} -> pure result
  let output = C8.unpack generated
  assertBool "expected sum constructor for id case" ("= UserById" `isInfixOf` output)
  assertBool "expected sum constructor for email case" ("| UserByEmail" `isInfixOf` output)
  assertBool "oneOf type should not be rendered as a record" (not $ "data UserBy = UserBy\n  {" `isInfixOf` output)
  assertBool "built-in @oneOf should not emit an unresolved Haskell directive reference" (not $ "typeDirective OneOf" `isInfixOf` output)

requireJust :: String -> Maybe a -> IO a
requireJust message = maybe (assertFailure message >> undefined) pure

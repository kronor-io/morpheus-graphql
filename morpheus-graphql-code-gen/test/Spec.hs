module Main
  ( main,
  )
where

import qualified CodeGen.OneOf as OneOf
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "morpheus-graphql-code-gen" [OneOf.tests]

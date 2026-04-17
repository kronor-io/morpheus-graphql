{-# LANGUAGE NoImplicitPrelude #-}

module Feature.Inference.TypeInferenceRendering
  ( testSchemaRendering,
  )
where

import qualified Data.ByteString.Lazy.Char8 as T
import Data.Morpheus.Server (printSchema)
import qualified Feature.Inference.TypeInference as TypeInference
import Relude hiding (ByteString)
import Test.Morpheus (FileUrl, file, mkUrl, renderingAssertion)
import Test.Tasty (TestTree)

renderSchema :: FileUrl -> IO (Either String T.ByteString)
renderSchema _ = pure $ Right $ printSchema TypeInference.proxy

testSchemaRendering :: TestTree
testSchemaRendering =
  renderingAssertion renderSchema (file (mkUrl "SchemaRendering") "type-inference")

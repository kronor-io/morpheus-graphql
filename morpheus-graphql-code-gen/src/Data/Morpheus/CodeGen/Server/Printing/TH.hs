{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.Printing.TH
  ( compileDocument,
    gqlDocument,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( CodeGenConfig (..),
    ServerDeclaration (..),
  )
import Data.Morpheus.CodeGen.Internal.AST
  ( TypeClassInstance (..),
    CodeGenTypeName (..),
    getFullName,
  )
import Data.Morpheus.CodeGen.Server.Interpreting.Transform
  ( parseServerTypeDefinitions,
  )
import Data.Morpheus.CodeGen.TH
  ( PrintDec (..),
    toName,
  )
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Relude hiding (ByteString, Type)

gqlDocument :: QuasiQuoter
gqlDocument = mkQuasiQuoter CodeGenConfig {namespace = False}

mkQuasiQuoter :: CodeGenConfig -> QuasiQuoter
mkQuasiQuoter ctx =
  QuasiQuoter
    { quoteExp = notHandled "Expressions",
      quotePat = notHandled "Patterns",
      quoteType = notHandled "Types",
      quoteDec = compileDocument ctx . pack
    }
  where
    notHandled things =
      error $ things <> " are not supported by the GraphQL QuasiQuoter"

compileDocument :: CodeGenConfig -> ByteString -> Q [Dec]
compileDocument config =
  parseServerTypeDefinitions config
    >=> fmap concat . traverse printServerDec . fst

isTypeDeclared :: CodeGenTypeName -> Q Bool
isTypeDeclared name = isJust <$> lookupTypeName (toString $ getFullName name)

isInstanceDefined :: Name -> CodeGenTypeName -> Q Bool
isInstanceDefined typeClass tName = do
  exists <- isTypeDeclared tName
  if exists
    then isInstance typeClass [ConT (toName tName)]
    else pure False

deriveIfNotDefined :: (TypeClassInstance a -> Q Dec) -> TypeClassInstance a -> Q [Dec]
deriveIfNotDefined f dec = do
  exists <- isInstanceDefined (typeClassName dec) (typeClassTarget dec)
  traverse f [dec | not exists]

printServerDec :: ServerDeclaration -> Q [Dec]
printServerDec (InterfaceType interface) = pure <$> printDec interface
printServerDec ScalarType {} = pure []
printServerDec (DataType dataType) = pure <$> printDec dataType
printServerDec (GQLTypeInstance _ gql) = deriveIfNotDefined printDec gql
printServerDec (GQLDirectiveInstance dir) = deriveIfNotDefined printDec dir

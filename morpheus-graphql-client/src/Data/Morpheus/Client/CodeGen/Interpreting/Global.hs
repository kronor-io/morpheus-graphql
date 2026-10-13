{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.CodeGen.Interpreting.Global
  ( toGlobalDefinitions,
  )
where

import Data.Morpheus.Client.CodeGen.AST
  ( ClientDeclaration,
    ClientPreDeclaration (..),
    ClientTypeDefinition (..),
    DERIVING_MODE (..),
  )
import Data.Morpheus.Client.CodeGen.Interpreting.Core (printClientType)
import Data.Morpheus.Client.CodeGen.Interpreting.PreDeclarations
  ( mapPreDeclarations,
  )
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConstructor (..),
    CodeGenField (..),
    CodeGenTypeName (..),
    FIELD_TYPE_WRAPPER (..),
    fromTypeName,
  )
import Data.Morpheus.CodeGen.Utils (Flag (FlagLanguageExtension), Flags)
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    DataEnumValue (DataEnumValue, enumName),
    FieldDefinition (..),
    Schema (Schema, types),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    TypeWrapper (..),
    VALID,
    hasOneOfDirective,
    isNotSystemTypeName,
    isNullable,
    isResolverType,
  )
import Relude hiding (empty)

toClientDeclarations :: ClientTypeDefinition -> [ClientPreDeclaration]
toClientDeclarations def@ClientTypeDefinition {clientKind}
  | KIND_SCALAR == clientKind = [FromJSONClass SCALAR_MODE cgType, ToJSONClass SCALAR_MODE cgType]
  | KIND_ENUM == clientKind = [ClientType cgType, FromJSONClass ENUM_MODE cgType, ToJSONClass ENUM_MODE cgType]
  | otherwise = [ClientType cgType, ToJSONClass TYPE_MODE cgType]
  where
    cgType = printClientType def

toGlobalDefinitions :: (TypeName -> Bool) -> Schema VALID -> GQLResult ([ClientDeclaration], Flags)
toGlobalDefinitions f Schema {types} = do
  let tyDefs = mapMaybe generateGlobalType $ filter shouldInclude (sortWith typeName $ toList types)
  decs <- traverse mapPreDeclarations $ concatMap toClientDeclarations tyDefs
  -- enums and oneOf input objects are encoded with \case
  let hasMatches = not $ null [x | x@ClientTypeDefinition {clientKind} <- tyDefs, clientKind == KIND_ENUM || isOneOf x]
  pure (decs, [FlagLanguageExtension "LambdaCase" | hasMatches])
  where
    shouldInclude t =
      not (isResolverType t)
        && isNotSystemTypeName (typeName t)
        && f (typeName t)

isOneOf :: ClientTypeDefinition -> Bool
isOneOf ClientTypeDefinition {clientKind = KIND_INPUT_OBJECT, clientCons = _ : _ : _} = True
isOneOf ClientTypeDefinition {clientKind = KIND_INPUT_OBJECT, clientCons = [CodeGenConstructor {constructorFields = [CodeGenField {fieldName = "_"}]}]} = True
isOneOf _ = False

generateGlobalType :: TypeDefinition ANY VALID -> Maybe ClientTypeDefinition
generateGlobalType TypeDefinition {typeName, typeContent, typeDirectives} = do
  (clientKind, clientCons) <- genContent typeContent
  pure
    ClientTypeDefinition
      { clientTypeName = fromTypeName typeName,
        clientKind,
        clientCons
      }
  where
    genContent :: TypeContent TRUE ANY VALID -> Maybe (TypeKind, [CodeGenConstructor])
    genContent (DataInputObject inputFields)
      | hasOneOfDirective typeDirectives =
          pure (KIND_INPUT_OBJECT, mkOneOfCons typeName <$> toList inputFields)
    genContent (DataInputObject inputFields) =
      pure
        ( KIND_INPUT_OBJECT,
          [ CodeGenConstructor
              { constructorName = fromTypeName typeName,
                constructorFields = toCodeGenField <$> toList inputFields
              }
          ]
        )
    genContent (DataEnum enumTags) = pure (KIND_ENUM, mkConsEnum typeName <$> enumTags)
    genContent DataScalar {} = pure (KIND_SCALAR, [])
    genContent _ = Nothing

toCodeGenField :: FieldDefinition a b -> CodeGenField
toCodeGenField FieldDefinition {fieldType = field@TypeRef {..}, ..} =
  CodeGenField
    { fieldName,
      fieldType = typeConName,
      wrappers = [GQL_WRAPPER typeWrappers],
      fieldIsNullable = isNullable field
    }

-- | each field of a oneOf input object is a constructor with a single non-nullable value.
-- e.g. @card: CardInput@ of @PaymentMethod@ becomes @PaymentMethodCard CardInput@.
mkOneOfCons :: TypeName -> FieldDefinition a b -> CodeGenConstructor
mkOneOfCons typename FieldDefinition {fieldName, fieldType = TypeRef {typeConName, typeWrappers}} =
  CodeGenConstructor
    { constructorName = CodeGenTypeName [coerce typename] [] (coerce fieldName),
      constructorFields =
        [ CodeGenField
            { fieldName = "_",
              fieldType = typeConName,
              wrappers = [GQL_WRAPPER (toRequired typeWrappers)],
              fieldIsNullable = False
            }
        ]
    }
  where
    toRequired (BaseType _) = BaseType True
    toRequired (TypeList elems _) = TypeList elems True

mkConsEnum :: TypeName -> DataEnumValue s -> CodeGenConstructor
mkConsEnum typename DataEnumValue {enumName} =
  CodeGenConstructor (CodeGenTypeName [coerce typename] [] enumName) []

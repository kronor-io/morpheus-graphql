{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST
  ( Ref (..),
    Position (..),
    Message,
    FieldName,
    Description,
    Stage,
    CONST,
    VALID,
    RAW,
    Value (..),
    ScalarValue (..),
    Object,
    replaceValue,
    decodeScientific,
    fromHaskellName,
    toHaskellName,
    RawValue,
    ValidValue,
    RawObject,
    ValidObject,
    ResolvedObject,
    ResolvedValue,
    Argument (..),
    Arguments,
    SelectionSet,
    SelectionContent (..),
    Selection (..),
    Fragments,
    Fragment (..),
    Operation (..),
    Variable (..),
    VariableDefinitions,
    DefaultValue,
    getOperationName,
    ScalarDefinition (..),
    DataEnum,
    FieldsDefinition,
    ArgumentDefinition (..),
    UnionTypeDefinition,
    ArgumentsDefinition,
    FieldDefinition (..),
    InputFieldsDefinition,
    TypeContent (..),
    TypeDefinition (..),
    Schema (..),
    TypeKind (..),
    TypeWrapper (..),
    TypeRef (..),
    DataEnumValue (..),
    OperationType (..),
    QUERY,
    MUTATION,
    SUBSCRIPTION,
    Directive (..),
    TypeCategory (..),
    VariableContent (..),
    TypeDefinitions,
    initTypeLib,
    kindOf,
    toNullable,
    isNullable,
    Subtyping (..),
    isNotSystemTypeName,
    isLeaf,
    isResolverType,
    mkEnumContent,
    createScalarType,
    mkUnionContent,
    mkTypeRef,
    mkInputUnionFields,
    fieldVisibility,
    lookupDeprecated,
    lookupDeprecatedReason,
    lookupWith,
    ExecutableDocument (..),
    Variables,
    unsafeFromFields,
    OrdMap (..),
    GQLError (..),
    GQLErrors,
    ObjectEntry (..),
    UnionTag (..),
    ANY,
    IN,
    OUT,
    OBJECT,
    IMPLEMENTABLE,
    fromAny,
    toAny,
    TRUE,
    FALSE,
    TypeName,
    Token,
    Msg (..),
    intercalate,
    fieldsToArguments,
    Directives,
    DirectivesDefinition,
    DirectiveDefinition (..),
    DirectiveLocation (..),
    FieldContent (..),
    fieldArguments,
    mkType,
    mkObjectField,
    UnionMember (..),
    mkUnionMember,
    mkNullaryMember,
    RawTypeDefinition (..),
    RootOperationTypeDefinition (..),
    UnionSelection,
    SchemaDefinition (..),
    buildSchema,
    getOperationDataType,
    Typed (Typed),
    typed,
    untyped,
    LEAF,
    INPUT_OBJECT,
    ToCategory (..),
    FromCategory (..),
    possibleTypes,
    possibleInterfaceTypes,
    mkField,
    defineSchemaWith,
    type (<=!),
    ToOBJECT,
    constraintInputUnion,
    getInputUnionValue,
    unitFieldName,
    unitTypeName,
    mkBaseType,
    mkMaybeType,
    isPossibleInterfaceType,
    packName,
    unpackName,
    at,
    atPositions,
    typeDefinitions,
    FragmentName,
    isInternal,
    internal,
    splitSystemSelection,
    lookupDataType,
  )
where

import Data.Mergeable.OrdMap (OrdMap (..))
import Data.Mergeable.SafeHashMap (SafeHashMap)
import Data.Morpheus.Types.Internal.AST.Base
import Data.Morpheus.Types.Internal.AST.DirectiveLocation (DirectiveLocation (..))
import Data.Morpheus.Types.Internal.AST.Error
import Data.Morpheus.Types.Internal.AST.Fields
import Data.Morpheus.Types.Internal.AST.Name
import Data.Morpheus.Types.Internal.AST.OperationType
import Data.Morpheus.Types.Internal.AST.Selection
import Data.Morpheus.Types.Internal.AST.Stage
import Data.Morpheus.Types.Internal.AST.Type
import Data.Morpheus.Types.Internal.AST.TypeCategory
import Data.Morpheus.Types.Internal.AST.TypeSystem
import Data.Morpheus.Types.Internal.AST.Union
import Data.Morpheus.Types.Internal.AST.Value
import Language.Haskell.TH.Syntax (Lift)
import Prelude (Show)

type Variables = SafeHashMap FieldName ResolvedValue

data ExecutableDocument = ExecutableDocument
  { inputVariables :: Variables,
    operation :: Operation RAW,
    fragments :: Fragments RAW
  }
  deriving (Show, Lift)

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Internal.Value
  ( DecodeRep (..),
    Context (..),
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Morpheus.App.Internal.Resolving (ResolverState)
import Data.Morpheus.Generic
  ( CProxy (..),
    DecodeFields,
    DecoderFun (..),
    DescribeCons,
    decodeFields,
    describeCons,
  )
import Data.Morpheus.Generic.Proxy (datatypeNameP)
import Data.Morpheus.Server.Deriving.Utils.Kinded (inputType)
import Data.Morpheus.Server.Deriving.Utils.Types (coerceInputObject, getField)
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving (..),
    UseGQLType (..),
    UseGQLValue (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    Object,
    ObjectEntry (..),
    TypeName,
    VALID,
    ValidValue,
    Value (..),
    getInputUnionValue,
    internal,
    msg,
  )
import GHC.Generics
import Relude

data Context = Context
  { isVariantRef :: Bool,
    typeName :: TypeName,
    enumVisitor :: TypeName -> TypeName,
    fieldVisitor :: FieldName -> FieldName,
    -- | for oneOf sum types: field name of a constructor, given the datatype name and the constructor name
    oneOfVisitor :: Maybe (TypeName -> TypeName -> FieldName),
    consDatatypeName :: TypeName
  }

type DecoderT = ReaderT Context ResolverState

setVariantRef :: Bool -> DecoderT a -> DecoderT a
setVariantRef isVariantRef = local (\ctx -> ctx {isVariantRef})

decideUnion ::
  (DecodeRep ctx f, DecodeRep ctx g) =>
  ctx ->
  ([TypeName], [TypeName]) ->
  TypeName ->
  ValidValue ->
  DecoderT ((f :+: g) a)
decideUnion drv (left, right) name value
  | name `elem` left = L1 <$> decodeRep drv value
  | name `elem` right = R1 <$> decodeRep drv value
  | otherwise =
      throwError
        $ internal
        $ "Constructor \""
        <> msg name
        <> "\" could not find in Union"

class DecodeRep ctx (f :: Type -> Type) where
  decodeRep :: ctx -> ValidValue -> DecoderT (f a)

instance (Datatype d, DecodeRep ctx f) => DecodeRep ctx (M1 D d f) where
  decodeRep drv value = M1 <$> local (\ctx -> ctx {consDatatypeName = datatypeNameP (Proxy @d)}) (decodeRep drv value)

instance (UseGQLType ctx gql, DescribeCons gql a, DescribeCons gql b, DecodeRep ctx a, DecodeRep ctx b) => DecodeRep ctx (a :+: b) where
  decodeRep ctx input = asks oneOfVisitor >>= maybe decodeUnion decodeOneOf
    where
      decodeOneOf toFieldName = do
        dtName <- asks consDatatypeName
        -- validated objects contain all fields: the omitted ones are null
        (name, value) <- case filter ((/= Null) . entryValue) . toList <$> coerceInputObject' input of
          Just [ObjectEntry {entryName, entryValue}] -> pure (entryName, entryValue)
          _ -> throwError (internal "oneOf input value must be an object with exactly one non-null field")
        let matches = any ((== name) . toFieldName dtName . fst)
        let left = describeCons (Proxy @a) :: [(TypeName, Maybe (CProxy gql))]
        let right = describeCons (Proxy @b) :: [(TypeName, Maybe (CProxy gql))]
        if
          | matches left -> L1 <$> decodeOneOfVariant ctx (length left == 1) input value
          | matches right -> R1 <$> decodeOneOfVariant ctx (length right == 1) input value
          | otherwise -> throwError $ internal $ "unknown oneOf field " <> msg name
      decodeUnion = do
        typename <- asks typeName
        let (kind, (left, right)) = getUnionTags ctx typename (Proxy @(a :+: b))
        (lr, name, value) <-
          case input of
            (Object obj) -> do
              (name, value) <- getInputUnionValue obj
              variant <- coerceInputObject value
              let isDone = [name] == left || [name] == left
              pure ((left, right), name, if isDone then Object variant else input)
            (Enum name) -> do
              visitor <- asks enumVisitor
              pure ((map visitor left, map visitor right), name, Enum name)
            _ -> throwError (internal "lists and scalars are not allowed in Union")
        setVariantRef kind (decideUnion ctx lr name value)

-- | once the constructor is found, its single field is decoded from the value of the oneOf field.
coerceInputObject' :: ValidValue -> Maybe (Object VALID)
coerceInputObject' (Object obj) = Just obj
coerceInputObject' _ = Nothing

decodeOneOfVariant :: (DecodeRep ctx f) => ctx -> Bool -> ValidValue -> ValidValue -> DecoderT (f a)
decodeOneOfVariant ctx isFound input value
  | isFound = setVariantRef True (decodeRep ctx value)
  | otherwise = decodeRep ctx input

instance (Constructor c, UseDeriving gql val ~ ctx, DecodeFields val a) => DecodeRep ctx (M1 C c a) where
  decodeRep ctx value = fmap M1 (decodeFields (decoder ctx value))

decoder :: (UseGQLValue ctx con) => ctx -> ValidValue -> DecoderFun con DecoderT
decoder ctx input =
  DecoderFun
    ( \name ->
        do
          Context {isVariantRef, fieldVisitor} <- ask
          value <- if isVariantRef then pure input else getField (fieldVisitor name) <$> coerceInputObject input
          lift (useDecodeValue ctx value)
    )

getUnionTags ::
  forall ctx f a b gql.
  (UseGQLType ctx gql, DescribeCons gql a, DescribeCons gql b) =>
  ctx ->
  TypeName ->
  f (a :+: b) ->
  (Bool, ([TypeName], [TypeName]))
getUnionTags ctx typename _ = do
  let left = map toInfo (describeCons (Proxy @a))
  let right = map toInfo (describeCons (Proxy @b))
  let varRef = find snd (left <> right)
  (isJust varRef, (map fst left, map fst right))
  where
    toInfo :: (TypeName, Maybe (CProxy gql)) -> (TypeName, Bool)
    toInfo (consName, Just (CProxy p))
      | consName == typename <> typeVariant = (typeVariant, True)
      | otherwise = (consName, False)
      where
        typeVariant = useTypename ctx (inputType p)
    toInfo (consName, Nothing) = (consName, False)

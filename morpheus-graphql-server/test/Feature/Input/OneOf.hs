{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Input.OneOf
  ( api,
  )
where

import Data.Morpheus.Server (interpreter)
import Data.Morpheus.Server.Types
  ( GQLRequest,
    GQLResponse,
    GQLType (..),
    OneOf (..),
    Rename (..),
    RootResolver (..),
    Undefined,
    defaultRootResolver,
    fieldDirective,
    typeDirective,
  )
import Data.Text
  ( Text,
    pack,
  )
import GHC.Generics (Generic)

newtype CardInput = CardInput
  { number :: Text
  }
  deriving (Generic, Show, GQLType)

-- every constructor is a field of the oneOf input object
data PaymentMethod
  = PaymentMethodCardInput CardInput
  | PaymentMethodSwish Text
  | PaymentMethodIds [Int]
  | PaymentMethodPhoneNumber (Maybe Text)
  | PaymentMethodLegacy Text
  deriving (Generic, Show)

instance GQLType PaymentMethod where
  directives _ =
    typeDirective OneOf
      <> fieldDirective "legacy" Rename {newName = "legacyID"}

-- a record of nullable fields
data Lookup = Lookup
  { byId :: Maybe Int,
    byName :: Maybe Text
  }
  deriving (Generic, Show)

instance GQLType Lookup where
  directives _ = typeDirective OneOf

-- types & args
newtype Arg a = Arg
  { value :: a
  }
  deriving (Generic, Show, GQLType)

-- query
testRes :: (Show a) => (Applicative m) => Arg a -> m Text
testRes Arg {value} = pure $ pack $ show value

-- resolver
data Query m = Query
  { pay :: Arg PaymentMethod -> m Text,
    payAll :: Arg [PaymentMethod] -> m Text,
    find :: Arg Lookup -> m Text
  }
  deriving (Generic, GQLType)

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  defaultRootResolver
    { queryResolver =
        Query
          { pay = testRes,
            payAll = testRes,
            find = testRes
          }
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver

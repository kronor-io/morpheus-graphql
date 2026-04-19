{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Directive.OneOfVisitor
  ( api,
  )
where

import Data.Kind (Type)
import Data.Morpheus.Server (interpreter)
import Data.Morpheus.Server.Types
  ( Arg (..),
    Describe (..),
    GQLRequest,
    GQLResponse,
    GQLType (..),
    Prefixes (..),
    Rename (..),
    RootResolver (..),
    Suffixes (..),
    Undefined,
    defaultRootResolver,
    fieldDirective',
    typeDirective,
  )
import Data.Text (Text, pack)
import GHC.Generics (Generic)

data UserSelectorInput
  = UserIdInput {idValue :: Int}
  | UserEmailInput {emailValue :: Text}
  | UserLegacyInput {legacyValue :: Text}
  deriving (Show, Generic)

instance GQLType UserSelectorInput where
  directives _ =
    typeDirective Describe {text = "Select a user by exactly one unique key"}
      <> typeDirective Prefixes {addPrefix = "", removePrefix = "User"}
      <> typeDirective Suffixes {addSuffix = "", removeSuffix = "Input"}
      <> fieldDirective' 'UserIdInput Describe {text = "Lookup by numeric id"}
      <> fieldDirective' 'UserEmailInput Describe {text = "Lookup by email address"}
      <> fieldDirective' 'UserLegacyInput Describe {text = "Lookup by legacy username"}
      <> fieldDirective' 'UserLegacyInput Rename {newName = "username"}

data Query (m :: Type -> Type) = Query
  { describe :: Arg "selector" UserSelectorInput -> m Text
  }
  deriving (Generic, GQLType)

root :: RootResolver IO () Query Undefined Undefined
root =
  defaultRootResolver
    { queryResolver =
        Query
          { describe = pure . pack . show . argValue
          }
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter root

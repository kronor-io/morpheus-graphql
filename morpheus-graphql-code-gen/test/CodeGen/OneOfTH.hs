{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CodeGen.OneOfTH
  ( constructorNames,
  )
where

import Data.Morpheus.CodeGen.Server
  ( CodeGenConfig (..),
    importServerTypeDefinitions,
  )
import Data.Morpheus.Server.CodeGen.Internal
import Data.Morpheus.Server.Types
import Relude

$(importServerTypeDefinitions CodeGenConfig {namespace = False} "test/fixtures/oneof/schema.graphql")

constructorNames :: [String]
constructorNames =
  [ case (UserById undefined :: UserBy) of
      UserById _ -> "UserById",
    case (UserByEmail undefined :: UserBy) of
      UserByEmail _ -> "UserByEmail"
  ]

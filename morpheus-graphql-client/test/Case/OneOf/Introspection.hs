{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- oneOf input objects from an introspection schema (__Type.isOneOf)
module Case.OneOf.Introspection where

import Data.Morpheus.Client (declareGlobalTypes)
import Relude
import Spec.Utils (path)

declareGlobalTypes (path "OneOf/schema.json")

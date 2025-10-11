{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server
  ( CodeGenConfig (..),
    PrinterConfig (..),
    gqlDocument,
    printServerDec,
    importServerTypeDefinitions,
  )
where

import Data.ByteString.Lazy.Char8
  ( readFile,
  )
import Data.FileEmbed (makeRelativeToProject)
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( CodeGenConfig (..),
  )
import Data.Morpheus.CodeGen.Server.Printing.TH
  ( compileDocument,
    gqlDocument,
    printServerDec,
  )
import Language.Haskell.TH (Dec, Q, runIO)
import Language.Haskell.TH.Syntax
  ( qAddDependentFile,
  )
import Relude hiding (ByteString, readFile)

newtype PrinterConfig = PrinterConfig
  { moduleName :: String
  }

importServerTypeDefinitions :: CodeGenConfig -> FilePath -> Q [Dec]
importServerTypeDefinitions ctx rawSrc = do
  src <- makeRelativeToProject rawSrc
  qAddDependentFile src
  runIO (readFile src)
    >>= compileDocument ctx

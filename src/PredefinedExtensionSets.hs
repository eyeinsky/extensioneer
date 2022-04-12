module PredefinedExtensionSets where

import Prelude

-- * Languages
--
-- | Data from
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/control.html
-- https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0380-ghc2021.rst

lang_GHC2021 :: [String]
lang_GHC2021 =
  [ "BangPatterns"
  , "BinaryLiterals"
  , "ConstrainedClassMethods"
  , "ConstraintKinds"
  , "DeriveDataTypeable"
  , "DeriveFoldable"
  , "DeriveFunctor"
  , "DeriveGeneric"
  , "DeriveLift"
  , "DeriveTraversable"
  , "DoAndIfThenElse"
  , "EmptyCase"
  , "EmptyDataDecls"
  , "EmptyDataDeriving"
  , "ExistentialQuantification"
  , "ExplicitForAll"
  , "FieldSelectors"
  , "FlexibleContexts"
  , "FlexibleInstances"
  , "ForeignFunctionInterface"
  , "GADTSyntax"
  , "GeneralisedNewtypeDeriving"
  , "HexFloatLiterals"
  , "ImplicitPrelude"
  , "ImportQualifiedPost"
  , "InstanceSigs"
  , "KindSignatures"
  , "MonomorphismRestriction"
  , "MultiParamTypeClasses"
  , "NamedFieldPuns"
  , "NamedWildCards"
  , "NumericUnderscores"
  , "PatternGuards"
  , "PolyKinds"
  , "PostfixOperators"
  , "RankNTypes"
  , "RelaxedPolyRec"
  , "ScopedTypeVariables"
  , "StandaloneDeriving"
  , "StandaloneKindSignatures"
  , "StarIsType"
  , "TraditionalRecordSyntax"
  , "TupleSections"
  , "TypeApplications"
  , "TypeOperators"
  , "TypeSynonymInstances"
  ]

lang_Haskell2010 :: [String]
lang_Haskell2010 =
  [ "CUSKs"
  , "DatatypeContexts"
  , "DoAndIfThenElse"
  , "EmptyDataDecls"
  , "FieldSelectors"
  , "ForeignFunctionInterface"
  , "ImplicitPrelude"
  , "MonomorphismRestriction"
  , "PatternGuards"
  , "RelaxedPolyRec"
  , "StarIsType"
  , "TraditionalRecordSyntax"
  ]

lang_Haskell98 :: [String]
lang_Haskell98 =
  [ "CUSKs"
  , "DatatypeContexts"
  , "FieldSelectors"
  , "ImplicitPrelude"
  , "MonomorphismRestriction"
  , "NPlusKPatterns"
  , "NondecreasingIndentation"
  , "StarIsType"
  , "TraditionalRecordSyntax"
  ]

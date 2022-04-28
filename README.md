# Extensioneer

```
extensioneer - Inspect extensions in cabal and hpack files

Usage: extensioneer [--ghc2021] [--haskell2010] [--haskell98]
                    paths to cabal or hpack files

Available options:
  --ghc2021                Include all extensions from GHC2021
  --haskell2010            Include all extensions from Haskell2010
  --haskell98              Include all extensions from Haskell98
  -h,--help                Show this help text
```

## Example usage

It takes a list of .cabal and package.yaml file paths as arguments and
prints out a matrix of all used extensions and in which files they are
used.

E.g running
```bash
git clone git@github.com:yesodweb/wai.git
extensioneer --ghc2021 $(find ./wai -name *.cabal -or -name package.yaml)
```

yields the following output
```yaml
# 0 - GHC2021
# 1 - ./wai/wai-app-static/wai-app-static.cabal
# 2 - ./wai/wai/wai.cabal
# 3 - ./wai/time-manager/time-manager.cabal
# 4 - ./wai/wai-extra/wai-extra.cabal
# 5 - ./wai/wai-http2-extra/wai-http2-extra.cabal
# 6 - ./wai/warp-quic/warp-quic.cabal
# 7 - ./wai/warp/warp.cabal
# 8 - ./wai/auto-update/auto-update.cabal
# 9 - ./wai/wai-websockets/wai-websockets.cabal
# 10 - ./wai/mime-types/mime-types.cabal
# 11 - ./wai/warp-tls/warp-tls.cabal
# 12 - ./wai/wai-conduit/wai-conduit.cabal
# 13 - ./wai/wai-frontend-monadcgi/wai-frontend-monadcgi.cabal

- BangPatterns                 # 0
- BinaryLiterals               # 0
- ConstrainedClassMethods      # 0
- ConstraintKinds              # 0
- DeriveDataTypeable           # 0
- DeriveFoldable               # 0
- DeriveFunctor                # 0
- DeriveGeneric                # 0
- DeriveLift                   # 0
- DeriveTraversable            # 0
- DoAndIfThenElse              # 0
- EmptyCase                    # 0
- EmptyDataDecls               # 0
- EmptyDataDeriving            # 0
- ExistentialQuantification    # 0
- ExplicitForAll               # 0
- FieldSelectors               # 0
- FlexibleContexts             # 0
- FlexibleInstances            # 0
- ForeignFunctionInterface     # 0
- GADTSyntax                   # 0
- GeneralisedNewtypeDeriving   # 0
- HexFloatLiterals             # 0
- ImplicitPrelude              # 0
- ImportQualifiedPost          # 0
- InstanceSigs                 # 0
- KindSignatures               # 0
- MonomorphismRestriction      # 0
- MultiParamTypeClasses        # 0
- NamedFieldPuns               # 0
- NamedWildCards               # 0
- NumericUnderscores           # 0
- OverloadedStrings            #         4
- PatternGuards                # 0
- PolyKinds                    # 0
- PostfixOperators             # 0
- RankNTypes                   # 0
- RelaxedPolyRec               # 0
- ScopedTypeVariables          # 0
- StandaloneDeriving           # 0
- StandaloneKindSignatures     # 0
- StarIsType                   # 0
- Strict                       #           5 6 7 8      11
- StrictData                   #           5 6 7 8      11
- TraditionalRecordSyntax      # 0
- TupleSections                # 0
- TypeApplications             # 0
- TypeOperators                # 0
- TypeSynonymInstances         # 0
```

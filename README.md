# Extensioneer

Inspect your cabal and hpack package extensions.

It takes a list of .cabal and package.yaml file paths as arguments and
prints out a matrix of all used extensions and in which files they are
used.

E.g running
```bash
git clone git@github.com:yesodweb/wai.git
extensioneer $(find ./wai -name *.cabal -or -name package.yaml)
```

yiels
```
# 0 - ./wai/wai-app-static/wai-app-static.cabal
# 1 - ./wai/wai/wai.cabal
# 2 - ./wai/time-manager/time-manager.cabal
# 3 - ./wai/wai-extra/wai-extra.cabal
# 4 - ./wai/wai-http2-extra/wai-http2-extra.cabal
# 5 - ./wai/warp-quic/warp-quic.cabal
# 6 - ./wai/warp/warp.cabal
# 7 - ./wai/auto-update/auto-update.cabal
# 8 - ./wai/wai-websockets/wai-websockets.cabal
# 9 - ./wai/mime-types/mime-types.cabal
# 10 - ./wai/warp-tls/warp-tls.cabal
# 11 - ./wai/wai-conduit/wai-conduit.cabal
# 12 - ./wai/wai-frontend-monadcgi/wai-frontend-monadcgi.cabal

- OverloadedStrings            #       3
- Strict                       #         4 5 6 7     10
- StrictData                   #         4 5 6 7     10
```

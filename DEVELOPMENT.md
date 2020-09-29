# Development Notes

## Deploy

Update .cabal file to new version

```
nix-shell -p cabal-install -p ghc
git tag v0.0.0.0
cabal haddock
cabal sdist

```

# WARNING: Josh is of no general interest until GHC Cmm JS backend is merged.

To run tests:

1. $ cabal configure --enable-tests && cabal build
2. Put right GHC build path into etc/bootstrap.cfg
3. $ ./dist/build/josh/josh --bootstrap etc/bootstrap.cfg
4. $ ./dist/build/Test/Test

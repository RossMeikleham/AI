#!/bin/bash
cabal configure
cabal install --only-dependencies
cabal build

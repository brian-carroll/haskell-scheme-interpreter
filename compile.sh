#!/bin/bash

stack build --ghc-options="-W -O2 $@"

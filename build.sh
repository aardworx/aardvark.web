#!/bin/bash

if [ -f .paket/Paket.Restore.targets ]; then
    mono .paket/paket.exe restore
fi

if [ -f node_modules ]; then
    npm install
fi

if [ -f paket-files/fable-compiler/Fable/build/fable-library ]; then
	npm run build-compiler
fi

npm run $@

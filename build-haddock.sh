#!/bin/bash

# Configurable Constants:

DOC_DIR='doc/html'
GHC_VER='9.8.1'
PKG_VER='0.1.0'

LIB_NAMES=(
	'alphabet'
	'dynamic-character'
	'dynamic-character-alignment'
	'dynamic-character-element'
	'measure-class'
	'measure-transition'
	'transition-matrix'
	'transition-matrix-ffi'
	'measure-units'
	'PhyloLib'
)

PKG_NAMES=("${LIB_NAMES[@]/#/PHANE-}")

# Function definitions
note() {
    local above="┍━━"
    local below="┕━━"
    local prefix="│ "
    local pattern="${above}\n%s\n${below}\n"
    echo -n -e "${1}" | sed "s/^/$prefix /" | xargs -0 printf ${pattern}
}

# Documentation generation:
mkdir -p "${DOC_DIR}"

note "Build individual package documentation"
cabal haddock --haddock-html --haddock-quickjump all


note "Gather together package documentation"
GEN_SUFFIX=''
for PKG in "${PKG_NAMES[@]}"; do
    cp -r "./dist-newstyle/build/aarch64-osx/ghc-${GHC_VER}/${PKG}-${PKG_VER}/${DOC_DIR}/${PKG}" "${DOC_DIR}";
    GEN_SUFFIX="${GEN_SUFFIX} --read-interface=${PKG},${DOC_DIR}/${PKG}/${PKG}.haddock"
done

note "Merge all package documentation"
haddock -o "${DOC_DIR}" --quickjump --gen-index --gen-contents ${GEN_SUFFIX}

note "PHANE documentation located: ${DOC_DIR}/index.html"

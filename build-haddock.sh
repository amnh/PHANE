#!/bin/bash

# Configurable Constants:

DOC_DIR='doc/html'
#LIB_NAMES=(
#	'alphabet'
#	'dynamic-character'
#	'dynamic-character-alignment'
#	'dynamic-character-element'
#        'file-formats'
#        'evaluation'
#	'measure-class'
#	'measure-transition'
#	'transition-matrix'
#	'transition-matrix-ffi'
#	'measure-units'
#	'PhyloLib'
#)

# Get the directory of the most recently build copy of the project.
DIR_PREFIX=$(find dist-newstyle/build/* -type d -name "ghc-*" -printf "%T@ %Tc %p\n" | sort -n --reverse | head -n 1 | cut -d' ' -f 7)

# Get the project version from the sub-packages directory name(s).
PKG_VER=$(ls "${DIR_PREFIX}" | head -n 1 | rev | cut -d'-' -f 1 | rev)

# Get the library names (without 'PHANE-' prefix) of all the packages.
LIB_NAMES=($(ls "${DIR_PREFIX}" | rev | cut -d'-' -f2- | rev | cut -d'-' -f2- | sort --ignore-case))

# Assign the package names as the library names *with* the 'PHANE-' prefix.
PKG_NAMES=("${LIB_NAMES[@]/#/PHANE-}")


# Function definitions
note() {
    local above="┍━━"
    local below="┕━━"
    local prefix="│ "
    local pattern="${above}\n%s\n${below}\n"
    echo -n -e "${1}" | sed "s/^/$prefix /" | xargs -0 printf ${pattern}
}


# Print script preamble
note "PHANE packages located:"
for LIB in "${LIB_NAMES[@]}"; do
    printf "  • %s\n" "${LIB}"
done
printf "\n"


# Documentation generation:
mkdir -p "${DOC_DIR}"

#note "Build individual package documentation"
#cabal haddock all \
#      --haddock-html \
#      --haddock-html-location='https://hackage.haskell.org/package/$pkg-$version/docs' \
#      --haddock-hyperlink-source \
#      --haddock-quickjump

note "Build individual package documentation"
cabal haddock-project all \
      --html \
      --local \
      --output "${DOC_DIR}/haddock-project-out"

note "Gather together package documentation"
GEN_SUFFIX=''
for PKG in "${PKG_NAMES[@]}"; do
    # Check for multiple public sublibraries
    SUBLIBS_EXIST=$(find "./${DIR_PREFIX}/${PKG}-${PKG_VER}" -maxdepth 1 -type d -name "doc")
    if [ -z "${SUBLIBS_EXIST}" ]
    then # There are one or more public sub-libraries
        SUBLIB_NAMES=($(ls "./${DIR_PREFIX}/${PKG}-${PKG_VER}/l"))
        for SUBLIB in "${SUBLIB_NAMES[@]}"; do
            SUBPKG="${PKG}-${SUBLIB}"
            cp -r "./${DIR_PREFIX}/${PKG}-${PKG_VER}/l/${SUBLIB}/${DOC_DIR}/${PKG}" "${DOC_DIR}/${SUBPKG}";
            GEN_SUFFIX="${GEN_SUFFIX} --read-interface=${SUBPKG},${DOC_DIR}/${SUBPKG}/${PKG}.haddock"
        done
    else # There are no (public) sub-libraries
        cp -r "./${DIR_PREFIX}/${PKG}-${PKG_VER}/${DOC_DIR}/${PKG}" "${DOC_DIR}";
        GEN_SUFFIX="${GEN_SUFFIX} --read-interface=${PKG},${DOC_DIR}/${PKG}/${PKG}.haddock"
    fi
done

note "Merge all package documentation"
haddock -o "${DOC_DIR}" --quickjump --gen-index --gen-contents ${GEN_SUFFIX}

note "PHANE documentation located: ${DOC_DIR}/index.html"

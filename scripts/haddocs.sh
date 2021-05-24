#!/bin/bash
# Build haddock documentation and an index for all projects in the repository.
#
# usage:
# ./haddocks.sh directory [true|false]
#
# $1 - where to put the generated pages, this directory contents will be wiped
#      out (so don't pass `/` or `./` - the latter will delete your 'dist-newstyle')
#      (the default is './haddocks')
# $2 - weahter to re-build haddocjs with `cabal haddock` command or a component name
#      (the default is true)
# $3 - cabal build directory
#      (the default is "dist-newstyle")

set -euo pipefail

OUTPUT_DIR=${1:-"./haddocks"}
REGENERATE=${2:-"true"}
BUILD_DIR=${3:-"dist-newstyle"}
DRY_RUN="${DRY_RUN:-}"

GHC_VERSION=$(ghc --numeric-version)
OS_ARCH="$(cat "$BUILD_DIR/cache/plan.json" | jq -r '.arch + "-" + .os' | head -n 1 | xargs)"

if [ "${DRY_RUN}" == 1 ]; then
  DRY_RUN_ARGS="--dry-run"
else
  DRY_RUN_ARGS=""
fi

# we don't include `--use-index` option, because then quickjump data is not
# generated.  This is not ideal, but there is no way to generate only top level
# `doc-index.html` file.  With this approach we get:
# * `doc-index.json` and `doc-index.html` per package
# * we can generate top level `doc-index.json` (which will only work at the top
#   level).
# * we could ammend package level `doc-index.json` files, but it's enough ...
#   this should be fixed upstream.
HADDOCK_OPTS=(
    --builddir "${BUILD_DIR}"
    --disable-optimization
    --haddock-all
    --haddock-internal
    --haddock-html
    --haddock-quickjump
    --haddock-hyperlink-source
    --haddock-option "--show-all"
    --haddock-option "--use-unicode"
    --disable-tests
    $DRY_RUN_ARGS
  )

# build documentation of all modules
if [ ${REGENERATE} == "true" ]; then
  cabal haddock "${HADDOCK_OPTS[@]}" \
    cardano-api \
    cardano-api-test \
    cardano-cli \
    cardano-config \
    cardano-node \
    hedgehog-extras \
    exe:cardano-cli \
    exe:cardano-node \
    exe:cardano-node-chairman \
    exe:cardano-testnet
elif [ ${REGENERATE} != "false" ]; then
  cabal haddock "${HADDOCK_OPTS[@]}" ${REGENERATE}
fi

if [ "${DRY_RUN}" == 1 ]; then
  echo "Exiting dry run"
  exit 0
fi

if [[ !( -d ${OUTPUT_DIR} ) ]]; then
  mkdir -p ${OUTPUT_DIR}
fi

# copy the new docs
for noopt_dir in $(find "${BUILD_DIR}/build/${OS_ARCH}/ghc-${GHC_VERSION}" -name noopt | grep -v /t/); do
  for doc_index in $(find "${noopt_dir}" -name doc-index.html); do
    package_dir="$(dirname "$doc_index")"
    package="$(echo "$(basename "${package_dir}")" | sed 's/-[0-9]\+\(\.[0-9]\+\)*//')"
    echo "Copying package: ${package}"
    cp -r "${package_dir}" "${OUTPUT_DIR}"
  done
done

# --read-interface options
interface_options () {
  for package in $(ls "${OUTPUT_DIR}"); do
    if [ -f "${OUTPUT_DIR}/${package}/${package}.haddock" ]; then
      echo "--read-interface=${package},${OUTPUT_DIR}/${package}/${package}.haddock"
    fi
  done
}

# Generate top level index using interface files
#
haddock \
  -o ${OUTPUT_DIR} \
  --title "cardano-node API" \
  --package-name "Cardano Node API" \
  --gen-index \
  --gen-contents \
  --quickjump \
  $(interface_options)

# Assemble a toplevel `doc-index.json` from package level ones.
#
echo "[]" > "${OUTPUT_DIR}/doc-index.json"
for file in $(ls $OUTPUT_DIR/*/doc-index.json); do
  project=$(basename $(dirname $file));
  jq -s \
    ".[0] + [.[1][] | (. + {link: (\"${project}/\" + .link)}) ]" \
    "${OUTPUT_DIR}/doc-index.json" \
    ${file} \
    > /tmp/doc-index.json
  mv /tmp/doc-index.json "${OUTPUT_DIR}/doc-index.json"
done

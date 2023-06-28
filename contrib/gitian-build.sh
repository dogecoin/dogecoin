#!/bin/bash

# Copyright (c) 2016 The Bitcoin Core developers
# Copyright (c) 2021 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

# Systems to build
DESCRIPTORS=('osx' 'win' 'linux')
SIGN_DESCRIPTORS=('win-signer' 'osx-signer')

# Gitian properties
export USE_DOCKER=0
export USE_LXC=0

# Dependencies
ossPatchUrl="https://depends.dogecoincore.org/osslsigncode-Backports-to-1.7.1.patch"
ossPatchHash="a8c4e9cafba922f89de0df1f2152e7be286aba73f78505169bc351a7938dd911"

ossTarUrl="https://depends.dogecoincore.org/osslsigncode_1.7.1.orig.tar.gz"
ossTarHash="f9a8cdb38b9c309326764ebc937cba1523a3a751a7ab05df3ecc99d18ae466c9"

macosSdkUrl="https://depends.dogecoincore.org/MacOSX10.11.sdk.tar.gz"
macosSdkHash="bec9d089ebf2e2dd59b1a811a38ec78ebd5da18cbbcd6ab39d1e59f64ac5033f"

liefUrl="https://depends.dogecoincore.org/lief-0.12.3-cp38-cp38-manylinux_2_17_x86_64.manylinux2014_x86_64.whl"
liefHash="c848aadac0816268aeb9dde7cefdb54bf24f78e664a19e97e74c92d3be1bb147"

# What to do
verify=false
build=false
buildSigned=false
commit=false
test=false

# Other Basic variables
SIGNER=
VERSION=
url=https://github.com/dogecoin/dogecoin
proc=2
mem=2000
scriptName=$(basename -- "$0")
outputDir=$(pwd)/gitian-output

# Help message
read -r -d '' usage <<-EOF
Usage: $scriptName [options] version

Standalone script to perform the gitian build of Dogecoin Core. Perform
deterministic build for multiples Operating System, using Docker, LXC or
KVM for virtualization. Sign binaries using PGP.

Uses https://github.com/devrandom/gitian-builder to execute the build.

Arguments:
version             Version number, commit, or branch to build. If building a
                    commit or branch, the -c option must be specified

Options:
--setup             Setup the gitian building environment.
-b|--build          Do the gitian build
-B|--build-signed   Build signed binaries for MacOS and Windows
-s|--sign name      Sign built executables with GPG using user ID
-v|--verify         Verify gitian built binaries
--lxc               Use LXC virtualization
--docker            Use Docker virtualization
-o|--os lwx         Specify which Operating Systems the build is for. Default is lwx,
                    l for Linux, w for Windows, x for MacOS
-j proc             Number of processes to use. Default $proc
-m n                Memory to allocate in MiB. Default $mem
-c|--commit         Indicate that the version argument is for a commit or branch
-u|--url repo       Specify the URL of the repository. Default is https://github.com/dogecoin/dogecoin
--test              CI TEST. Uses Docker
-h|--help           Print this help message
EOF

# Get options and arguments
while :; do
    case $1 in
        # Verify
        -v | --verify)
            verify=true
            ;;
        # Build
        -b | --build)
            build=true
            ;;
        # Build signed binaries
        -B | --build-signed)
            buildSigned=true
            ;;
        # PGP Signer
        -s | --sign)
            if [ -n "$2" ]; then
                SIGNER=$2
                shift
            else
                echo 'Error: "--sign" requires a PGP signer.'
                exit 1
            fi
            ;;
        # Operating Systems
        -o | --os)
            if [ -n "$2" ]; then
                DESCRIPTORS=()
                SIGN_DESCRIPTORS=()
                if [[ "$2" == *"l"* ]]; then
                    DESCRIPTORS+=('linux')
                fi
                if [[ "$2" == *"w"* ]]; then
                    DESCRIPTORS+=('win')
                    SIGN_DESCRIPTORS+=('win-signer')
                fi
                if [[ "$2" == *"x"* ]]; then
                    DESCRIPTORS+=('osx')
                    SIGN_DESCRIPTORS+=('osx-signer')
                fi
                shift
            else
                echo 'Error: "--os" specify os: l (linux), w (windows), or x (Mac OSX)'
                exit 1
            fi
            ;;
        # Help message
        -h | --help)
            echo "$usage"
            exit 0
            ;;
        # Commit or branch
        -c | --commit)
            commit=true
            ;;
        # Number of Processes
        -j)
            if [ -n "$2" ]; then
                proc=$2
                shift
            else
                echo 'Error: "-j" requires an argument'
                exit 1
            fi
            ;;
        # Memory to allocate
        -m)
            if [ -n "$2" ]; then
                mem=$2
                shift
            else
                echo 'Error: "-m" requires an argument'
                exit 1
            fi
            ;;
        # lxc
        --lxc)
            USE_LXC=1
            export LXC_BRIDGE=${LXC_BRIDGE:-br0}
            ;;
        # docker
        --docker)
            USE_DOCKER=1
            ;;
        # URL
        -u)
            if [ -n "$2" ]; then
                url=$2
                shift
            else
                echo 'Error: "-u" requires an argument'
                exit 1
            fi
            ;;
        # Setup
        --setup)
            setup=true
            ;;
        --test)
            test=true
            ;;
        *) # Default case: If no more options then break out of the loop.
            break ;;
    esac
    shift
done

# Download specific file and verify hash
function download_file () {
    local filename=$(basename $1)

    if [ ! -f $filename ]; then
        wget $1
    fi

    # Verify file signature
    echo "$2 $filename" | sha256sum -c --status

    if [ $? != 0 ]; then
        echo "$scriptName: Signature for $filename don't match."
        exit 1
    fi
}

function move_build_files() {
    find build/out -type f -exec mv '{}' $outputDir/dogecoin-binaries/${VERSION}/ \;
}

function download_descriptor() {
    if [[ ! $1 == 'test' ]]; then
        uri="${url/github.com/raw.githubusercontent.com}"/"$2"/contrib/gitian-descriptors/gitian-"$1".yml
        echo "Downloading descriptor ${1} ${uri}"
        wget $uri -O gitian-"$1".yml || exit 1
    else
        # CI tests
        cp ../ci/descriptor/"$1".yml gitian-"$1".yml || exit 1
    fi
}

### Test configuration ###

if [[ $test == true ]]; then
    if [[ $commit == true ]]; then
        VERSION="f80bfe9068ac1a0619d48dad0d268894d926941e"
    else
        VERSION="1.14.3"
    fi
    DESCRIPTORS=('test')
    SIGN_DESCRIPTORS=()
    COMMIT=$VERSION
    SIGNER="signer"
    SHA256SUM="0d519f6ade0e601617a7c44b764eeae35a8784070c3e44f542011956f1743459"
fi

### Arguments checks ####

echo "Using ${proc} CPU and ${mem} RAM"

# Control the selection of a single virtualisation software
if [ $(($USE_LXC + $USE_DOCKER)) -ge 2 ]; then
    echo "$scriptName: Specify a single virtualisation solution between Docker, LXC or KVM."
    exit
fi

# Get version
if [ -n "$1" ]; then
    VERSION=$1

    # Use tag or commit within repository
    if [[ $commit == false ]]; then
        COMMIT=v$VERSION
    else
        COMMIT=$VERSION
    fi
else
    echo "$scriptName: Missing version, see --help for more information."
    exit 1
fi

### Setup ###

if [[ $setup == true ]]; then
    git clone https://github.com/dogecoin/gitian.sigs.git
    git clone https://github.com/dogecoin/dogecoin-detached-sigs.git
    git clone https://github.com/devrandom/gitian-builder.git

    pushd ./gitian-builder

    #Download dependencies
    mkdir -p inputs
    pushd inputs

    download_file $ossPatchUrl $ossPatchHash
    download_file $ossTarUrl $ossTarHash
    download_file $macosSdkUrl $macosSdkHash
    download_file $liefUrl $liefHash

    popd

    #Prepare containers depending of virtualization solution: lxc, docker, kvm
    if [ "$USE_LXC" -eq 1 ]
    then
        sudo apt-get install -y lxc
        bin/make-base-vm --suite focal --arch amd64 --lxc
    elif [ "$USE_DOCKER" -eq 1 ]; then
        bin/make-base-vm --suite focal --arch amd64 --docker
    else
        bin/make-base-vm --suite focal --arch amd64
    fi
    popd
fi

# Download descriptors
mkdir -p ./gitian-descriptors/

pushd gitian-descriptors || exit 1

if [[ $build == true || $verify == true ]]; then
    for descriptor in "${DESCRIPTORS[@]}"; do
        download_descriptor "$descriptor" "$COMMIT"
    done
fi

if [[ $buildSigned == true ]]; then
    for sign_descriptor in "${SIGN_DESCRIPTORS[@]}"; do
        download_descriptor "$sign_descriptor" "$COMMIT"
    done
fi

popd

### Build ###

if [[ $build == true ]]; then
    # Make output folder
    mkdir -p $outputDir/dogecoin-binaries/"$VERSION"

    pushd ./gitian-builder || exit 1

    # Clean dogecoin git directory because of old caching
    if [ -d inputs/dogecoin/ ]; then
        echo "Cleaning Dogecoin directory..."
        rm -rf inputs/dogecoin/
    fi

    for descriptor in "${DESCRIPTORS[@]}"; do
        echo ""
        echo "Compiling ${VERSION} ${descriptor}"
        echo ""
        ./bin/gbuild -j "$proc" -m "$mem" --commit dogecoin="$COMMIT" --url dogecoin="$url" ../gitian-descriptors/gitian-"$descriptor".yml  || exit 1

        if [ -n "$SIGNER" ]; then
            ./bin/gsign --signer "$SIGNER" --release "$VERSION"-"$descriptor" \
                --destination $outputDir/sigs/ ../gitian-descriptors/gitian-"$descriptor".yml 2>&- || \
                echo "$0: Error on signature, detached signing"
        fi
        move_build_files
    done

    popd  || exit 1
fi

# Build signed binaries
if [[ $buildSigned == true ]]; then
    pushd gitian-builder || exit 1

    for sign_descriptor in "${SIGN_DESCRIPTORS[@]}"; do
        echo ""
        echo "Compiling Binary ${VERSION} ${sign_descriptor}"
        echo ""
        ./bin/gbuild --skip-image --upgrade --commit signature="$COMMIT" ../gitian-descriptors/gitian-"$sign_descriptor".yml || exit 1
        if [ -n "$SIGNER" ]; then
            ./bin/gsign --signer "$SIGNER" --release "$VERSION"-"$sign_descriptor" \
                --destination $outputDir/sigs/ ../gitian-descriptors/gitian-"$sign_descriptor".yml
        fi
        move_build_files
    done

    popd  || exit 1
fi

### Signatures Verification ###

if [[ $verify == true ]]; then
    pushd ./gitian-builder || exit 1

    for descriptor in "${DESCRIPTORS[@]}"; do
        echo ""
        echo "Verifying v${VERSION} ${descriptor}"
        echo ""
        ./bin/gverify -v -d ../gitian.sigs/ -r "${VERSION}"-"$descriptor" ../gitian-descriptors/gitian-"$descriptor".yml
    done

    popd  || exit 1
fi

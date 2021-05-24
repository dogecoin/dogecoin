# Building the development version of the Cardano node on Windows

This document explains how to build a __DEVELOPMENT__ version of the `cardano-node`.
Note that this is *not* for building a __PRODUCTION__ version of the node. 

To start building on `Windows`, you will need to install and configure specific tools outlined below. We recommend using [chocolatey](https://chocolatey.org), which provides the `choco` command to install these tools.  

You can run all the instructions that invoke `choco` in PowerShell with root privileges. We recommend installing and using `git-bash` for development purposes, which is `git` when installed with `choco install git`.

## Install GHC

The recommended way is to run the following command in PowerShell:

```PowerShell
choco install --version 8.10.2 ghc
```

## Install pkg-config

```PowerShell
choco install pkgconfiglite
```

## Install vcpkg

You will first need to install `vcpkg` to proceed with `libsodium` library installation (which is the next step). 

For this, use `git`(which you can also install using `choco`) and follow [these
instructions](https://github.com/microsoft/vcpkg#quick-start-windows).

You can now install `libsodium` with the following command:
```bash
./vcpkg install --triplet x64-windows libsodium
```

## Create libsodium.pc file

To find system dependencies like `libsodium`, `cabal` uses `pkg-config`. On Windows, you will need to create the `libsodium.pc` description file in a correct
directory.

In one of the paths reported by:
```bash
pkg-config --variable pc_path pkg-config
```
create `libsodium.pc` file, which contains:

```
libdir=VCPKG_PATH/installed/x64-windows/bin
includedir=VCPKG_PATH/installed/x64-windows/include

Name: libsodium
Description: libsodium library
Cflags: -I${includedir}/sodium
Libs: -L${libdir} -llibsodium
```
> Note that you need to replace `VCPKG_PATH` with the
absolute path, where you use `vcpkg`. Please verify that the paths above contain
`libsodium.dll` file and headers.

> Also, you cannot use `prefix=` in the `libsodium.pc` file. This might be changed for
some other directory, `pkg-config` provides a switch to use the provided
`prefix`, but there is no way to instruct `cabal` to do so.

## `cabal` configuration

Go to the directory, where you cloned the `cardano-node` repository and add the command below
to your `cabal.project.local` file (if you don't already have it, create one):

```
max-backjump: 5000
reorder-goals: True

package cardano-crypt-praos
  flags: -external-libsodium-vrf

extra-lib-dirs: "VCPKG_PATH\\installed\\x64-windows\\bin"
extra-include-dirs: "VCPKG_PATH\\installed\\x64-windows\\include"
```

The final step is to add `VCPKG_PATH/installed/x64-windows/bin` to the `PATH`
variable. Since we are using `git-bash`, you can do this with: 

```
export PATH="VCPKG_PATH/installed/x64-windows/bin:${PATH}"
```
*Remember to substitute `VCPKG_PATH` with the real path.*

You can now build the node with:

```bash
cabal build --builddir /c/dist exe:cardano-node
```
> Note: using `--builddir /c/dist` with a succinct directory protects you
from exceeding the [maximal path
size](https://docs.microsoft.com/en-us/windows/win32/fileio/maximum-file-path-limitation)
on Windows (which is a limitation of the linker rather than `ghc` itself).

You can now verify whether the node runs: 
```bash
cabal run --builddir /c/dist exe:cardano-node -- run --help
```

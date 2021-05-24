# Installing the node from source

The **latest** version of the node may be downloaded from the [cardano-node GitHub Releases](https://github.com/input-output-hk/cardano-node/releases) page.

#### Prerequisites

To set up your platform, you will need:

* An x86 host \(AMD or Intel\)
* A virtual machine or AWS instance with at least 2 cores
* 8GB of RAM and at least 10GB of free disk space
* A recent version of Linux

**Note** The node can be built and run on other operating systems, including Windows and MacOSX, but we recommend that
stake pool operators use Linux to take advantage of the associated performance advantages. If you are building on Windows, we recommend using WSL2 under Windows 10 as this provides a development and execution environment that is very similar to Ubuntu.

#### Installation dependencies

To download the source code and build it, you need the following packages and tools on your Linux system:

* the version control system `git`,
* the `gcc` C-compiler,
* C++ support for `gcc`,
* developer libraries for the arbitrary precision library `gmp`,
* developer libraries for the compression library `zlib`,
* developer libraries for `systemd`,
* developer libraries for `ncurses`,
* `ncurses` compatibility libraries,
* the Haskell build tool `cabal`,
* the GHC Haskell compiler (version `8.10.2` or above).

In Redhat, Fedora, and Centos:

    sudo yum update -y
    sudo yum install git gcc gcc-c++ tmux gmp-devel make tar xz wget zlib-devel libtool autoconf -y
    sudo yum install systemd-devel ncurses-devel ncurses-compat-libs -y

For Debian/Ubuntu, use the following instead:

    sudo apt-get update -y
    sudo apt-get install automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf -y

If you are using a different flavor of Linux, you will need to use the correct package manager for your platform instead of `yum` or `apt-get`, and the names of the packages you need to install might differ.  On MacOSX, use the Homebrew (`brew`) installer.

#### Downloading, unpacking, installing, and updating Cabal:

    Download the relevant `cabal-install` (version 3.4.0.0) binary from https://www.haskell.org/cabal/download.html
    tar -xf cabal-install-3.4.0.0-xxxxxx.tar.xz
    rm cabal-install-3.4.0.0-xxxxxx.tar.xz
    mkdir -p ~/.local/bin
    mv cabal ~/.local/bin/

Verify that ~/.local/bin is in your PATH:

    echo $PATH

If `~/.local/bin` is not in the PATH, you need to add the following line to  your `.bashrc` file

    export PATH="~/.local/bin:$PATH"

and source the file:

    source .bashrc

Update cabal:

    cabal update

Confirm that you installed cabal version `3.4.0.0`:

    cabal --version

**Note:** We no longer provide supported `stack` or `nix` installer packages. We recommend using `cabal` instead.


#### Downloading and installing the GHC compiler:

Create a working directory for your builds:

    mkdir -p ~/src
    cd ~/src

 Download and install the latest version of GHC:

    wget https://downloads.haskell.org/ghc/8.10.2/ghc-8.10.2-x86_64-deb9-linux.tar.xz
    tar -xf ghc-8.10.2-x86_64-deb9-linux.tar.xz
    rm ghc-8.10.2-x86_64-deb9-linux.tar.xz
    cd ghc-8.10.2
    ./configure
    sudo make install

This assumes GHC 8.10.2 on Linux (the most recent version at the time of writing).  If you are installing on MacOSX or Windows, download the compiler from `https://www.haskell.org/platform/mac.html` instead, and follow the installation instructions. Note that using a newer version than the one specified may produce compilation errors.

#### Installing Libsodium

Create a working directory for your builds:

    mkdir -p ~/src
    cd ~/src

Download and install libsodium:

    git clone https://github.com/input-output-hk/libsodium
    cd libsodium
    git checkout 66f017f1
    ./autogen.sh
    ./configure
    make
    sudo make install

Add the following to your .bashrc file and source it:

    export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
    export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"

#### Downloading the source code for cardano-node

Create a working directory for your builds:

    mkdir -p ~/src
    cd ~/src

Download the Cardano node sources:

    git clone https://github.com/input-output-hk/cardano-node.git

Change the working directory to the downloaded source code folder:

    cd cardano-node


Checkout the latest version of cardano-node (choose the tag with the highest version number: ``TAGGED-VERSION``):

    git fetch --all --recurse-submodules --tags
    git tag
    git checkout tags/<TAGGED VERSION>

#### Configuring the build options

We explicitly use the GHC version that we installed earlier.  This avoids defaulting to a system version of GHC that might be older than the one you have installed.

    cabal configure --with-compiler=ghc-8.10.2

Update the local project file to use the VRF library that you installed earlier.

    echo "package cardano-crypto-praos" >>  cabal.project.local
    echo "  flags: -external-libsodium-vrf" >>  cabal.project.local


#### Building and installing the node

Build the node and CLI with `cabal`:

    cabal build all

Install the newly built node and CLI commands to the `~/.local/bin` directory:

    cp -p "$(./scripts/bin-path.sh cardano-node)" ~/.local/bin/
    cp -p "$(./scripts/bin-path.sh cardano-cli)" ~/.local/bin/

Note, we avoid using `cabal install` because that method prevents the installed binaries from reporting
the git revision with the `--version` switch.

Check the version that has been installed:

    cardano-cli --version

Repeat the above process when you need to update to a new version.


**Note:** It might be necessary to delete the `db`-folder \(the database-folder\) before running an updated version of the node.

WINDOWS BUILD NOTES
====================

Below are some notes on how to build Dogecoin Core for Windows.

Most developers use cross-compilation from Ubuntu to build executables for
Windows. This is also used to build the release binaries.

While there are potentially a number of ways to build on Windows (for example using msys / mingw-w64),
using the Windows Subsystem For Linux is the most straightforward. If you are building with
another method, please contribute the instructions here for others who are running versions
of Windows that are not compatible with the Windows Subsystem for Linux.

Compiling with Windows Subsystem For Linux
-------------------------------------------

With Windows 10, Microsoft has released a new feature named the [Windows
Subsystem for Linux](https://msdn.microsoft.com/commandline/wsl/about). This
feature allows you to run a bash shell directly on Windows in an Ubuntu-based
environment. Within this environment you can cross compile for Windows without
the need for a separate Linux VM or server.

This feature is not supported in versions of Windows prior to Windows 10 or on
Windows Server SKUs. In addition, it is available [only for 64-bit versions of
Windows](https://msdn.microsoft.com/en-us/commandline/wsl/install_guide).

To get the bash shell, you must first activate the feature in Windows.

1. Enable the Windows Subsystem for Linux feature
  * Open the Windows Features dialog (`OptionalFeatures.exe`)
  * Enable 'Windows Subsystem for Linux'
  * Click 'OK' and restart if necessary
2. Install Ubuntu
  * Open Microsoft Store and search for "Ubuntu 18.04" or use [this link](https://www.microsoft.com/store/productId/9N9TNGVNDL3Q)
  * Click Install
3. Complete Installation
  * Open a cmd prompt and type "Ubuntu1804"
  * Create a new UNIX user account (this is a separate account from your Windows account)

After the bash shell is active, you can follow the instructions below, starting
with the "Cross-compilation" section. Compiling the 64-bit version is
recommended but it is possible to compile the 32-bit version.

Cross-compilation
-------------------

These steps can be performed on, for example, an Ubuntu VM. The depends system
will also work on other Linux distributions, however the commands for
installing the toolchain will be different.

First, install the general dependencies:

    sudo apt update
    sudo apt upgrade
    sudo apt-get install build-essential libtool autotools-dev automake pkg-config bsdmainutils curl git
    
If you want to build with the wallet and Qt GUI you also want to install the following (this example is under Ubuntu):

    sudo apt-get install libssl-dev libboost-all-dev qt5-default libprotobuf-dev libqrencode4 libdb++-dev libdb-dev miniupnpc

A host toolchain (`build-essential`) is necessary because some dependency
packages (such as `protobuf`) need to build host utilities that are used in the
build process.

## Building for 64-bit Windows

To build executables for Windows 64-bit, install the following dependencies:

    sudo apt-get install g++-mingw-w64-x86-64

For Ubuntu 18.04 and 20.04, set the default mingw32 g++ compiler option to posix:

    sudo update-alternatives --config x86_64-w64-mingw32-g++ 

...Choose the "posix" (vs 'auto' or 'win32') option, and continue.

Note that for WSL v1 the Dogecoin Core source path MUST be somewhere in the default mount file system, for
example /usr/src/dogecoin, AND not under, for example, /mnt/d/dogecoin. 

If this is not the case the dependency autoconf scripts will fail (silently.)
This means you cannot use a directory that is located directly on the host Windows file system to perform the build.

If using WSL 1, you'll need to turn off WSL Support for Win32 applications temporarily, or you will get ABI errors and format errors for some .o files.

If using WSL 1 then build using:

    PATH=$(echo "$PATH" | sed -e 's/:\/mnt.*//g') # strip out problematic Windows %PATH% imported var
    sudo bash -c "echo 0 > /proc/sys/fs/binfmt_misc/status" # Temporarily Disable WSL support for Win32 applications.
    cd depends
    make HOST=x86_64-w64-mingw32
    cd ..
    ./autogen.sh
    CONFIG_SITE=$PWD/depends/x86_64-w64-mingw32/share/config.site ./configure --prefix=/
    make
    sudo bash -c "echo 1 > /proc/sys/fs/binfmt_misc/status" # Re-Enable WSL support for Win32 applications.
    
If using WSL 2 then you should be able to build just with:

    cd depends
    make HOST=x86_64-w64-mingw32
    cd ..
    ./autogen.sh # not required when building from tarball
    CONFIG_SITE=$PWD/depends/x86_64-w64-mingw32/share/config.site ./configure --prefix=/
    make
    
## Building for 32-bit Windows

To build executables for Windows 32-bit, install the following dependencies:

    sudo apt-get install g++-mingw-w64-i686 mingw-w64-i686-dev 

Then build using:

    cd depends
    make HOST=i686-w64-mingw32
    cd ..
    ./autogen.sh # not required when building from tarball
    CONFIG_SITE=$PWD/depends/i686-w64-mingw32/share/config.site ./configure --prefix=/
    make

## Depends system

For further documentation on the depends system see [README.md](../depends/README.md) in the depends directory.

Installation
-------------

After building using the Windows subsystem it can be useful to copy the compiled
executables to a directory on the windows drive in the same directory structure
as they appear in the release `.zip` archive. This can be done in the following
way. This will install to `c:\workspace\dogecoin`, for example:

    make install DESTDIR=/mnt/c/workspace/dogecoin

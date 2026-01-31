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
  * Open Microsoft Store and search for "Ubuntu 20.04" or use [this link](https://apps.microsoft.com/detail/9mttcl66cpxj)
  * Click Install
  * Reboot (if prompted)
3. Complete Installation
  * Open a cmd prompt and type "Ubuntu2004"
  * Create a new UNIX user account (this is a separate account from your Windows account)

After the bash shell is active, you can follow the instructions below, starting
with the "Cross-compilation" section. Compiling the 64-bit version is
recommended but it is possible to compile the 32-bit version.

Some people have reported trouble with these steps and recommend that you apply all relevant Windows updates. Also be sure
to enable virtualization in your BIOS if necessary.

Cross-compilation
-------------------

These steps can be performed on, for example, an Ubuntu VM. The depends system
will also work on other Linux distributions, however the commands for
installing the toolchain will be different.

Install the general dependencies. First, ensure your system is updated and has the latest security patches.

    sudo apt update
    sudo apt upgrade
    sudo apt-get install build-essential libtool autotools-dev automake pkg-config bsdmainutils curl git

A host toolchain (`build-essential`) is necessary because some dependency
packages (such as `protobuf`) need to build host utilities that are used in the
build process.

## Get the Source Code

To build Dogecoin from source code, you'll need the source code. Either check it out via `git` or download
a zip file. (Look at the green "<> Code" button on [the Dogecoin GitHub repository](https://github.com/dogecoin/dogecoin/)).

Make sure this code is available in your Ubuntu directory. If you've unzipped a single downloaded file, you may need to change
the permissions of all extracted files with command like:

  sudo chmod -R <your_username> .

If you've downloaded via `git`, do not use `sudo`. Instead prefer something like:

    cd $HOME
    git clone https://github.com/dogecoin/dogecoin.git
    git checkout <branchname>

... where `<branchname>` is the name of the branch you want to build, such as
"1.14.7-dev" for the unstable branch or "master" for the most recent stable
release.

## Building for 64-bit Windows

To build executables for Windows 64-bit, install the following dependencies:

    sudo apt-get install g++-mingw-w64-x86-64

For Ubuntu 20.04, set the default mingw32 g++ compiler option to posix:

    sudo update-alternatives --config x86_64-w64-mingw32-g++

...Choose the "posix" (vs 'auto' or 'win32') option, and continue.

Note that for WSL v1 the Dogecoin Core source path MUST be somewhere in the default mount file system, for
example /usr/src/dogecoin, AND not under, for example, /mnt/d/dogecoin.

If this is not the case the dependency autoconf scripts will fail (silently.)
This means you cannot use a directory that is located directly on the host Windows file system to perform the build.

To identify your version of WSL, run the command:

  wsl -l -v

If using WSL 1, you'll need to turn off WSL Support for Win32 applications temporarily, or you will get ABI errors and format errors for some .o files.

If using WSL 1 then build using:

    PATH=$(echo "$PATH" | sed -e 's/:\/mnt.*//g') # strip out problematic Windows %PATH% imported var
    sudo bash -c "echo 0 > /proc/sys/fs/binfmt_misc/status" # Temporarily Disable WSL support for Win32 applications.
    cd depends
    make HOST=x86_64-w64-mingw32
    cd ..
    ./autogen.sh # not required when building from release tarball
    CONFIG_SITE=$PWD/depends/x86_64-w64-mingw32/share/config.site ./configure --prefix=/
    make
    sudo bash -c "echo 1 > /proc/sys/fs/binfmt_misc/status" # Re-Enable WSL support for Win32 applications.

If using WSL 2 then you should be able to build just with:

    PATH=$(echo "$PATH" | sed -e 's/:\/mnt.*//g') # strip out problematic Windows %PATH% imported var
    cd depends
    make HOST=x86_64-w64-mingw32
    cd ..
    ./autogen.sh # not required when building from release tarball
    CONFIG_SITE=$PWD/depends/x86_64-w64-mingw32/share/config.site ./configure --prefix=/
    make

In either case, be aware that if the first `make` invocation fails, you will see further errors during the `./configure` and second `make` stages. Please
report any errors with this in mind.

Also be careful using the `-j` flag with any `make` command. People have reported strange compilation errors when doing so.

## Building for 32-bit Windows

To build executables for Windows 32-bit, install the following dependencies:

    sudo apt-get install g++-mingw-w64-i686 mingw-w64-i686-dev

Ensure that this toolchain can build for `posix` (else you may have compilation errors for Dogecoin's dependencies):

    sudo update-alternatives --config i686-w64-mingw32-g++

Choose the `posix` option (instead of `auto` or `win32`), and continue. Then build using:

    cd depends
    make HOST=i686-w64-mingw32
    cd ..
    ./autogen.sh # not required when building from tarball
    CONFIG_SITE=$PWD/depends/i686-w64-mingw32/share/config.site ./configure --prefix=/
    make

## Depends system

For further documentation on the depends system see [README.md](../depends/README.md) in the `depends/` directory.

Installation
-------------

After building using the Windows subsystem it can be useful to copy the compiled
executables to a directory on the windows drive in the same directory structure
as they appear in the release `.zip` archive. This can be done in the following
way. This will install to `c:\workspace\dogecoin`, for example:

    make install DESTDIR=/mnt/c/workspace/dogecoin

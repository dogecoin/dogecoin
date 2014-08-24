Mac OS X Build Instructions and Notes
====================================
This guide will show you how to build dogecoind(headless client) for OSX.

Notes
-----

* Tested on OS X 10.6 through 10.9 on 64-bit Intel processors only.
Older OSX releases or 32-bit processors are no longer supported.

* All of the commands should be executed in a Terminal application. The
built-in one is located in `/Applications/Utilities`.

Preparation
-----------

You need to install XCode with all the options checked so that the compiler
and everything is available in /usr not just /Developer. XCode should be
available on your OS X installation media, but if not, you can get the
current version from https://developer.apple.com/xcode/. If you install
Xcode 4.3 or later, you'll need to install its command line tools. This can
be done in `Xcode > Preferences > Downloads > Components` and generally must
be re-done or updated every time Xcode is updated.

There's an assumption that you already have `git` installed, as well. If
not, it's the path of least resistance to install [Github for Mac](https://mac.github.com/)
(OS X 10.7+) or
[Git for OS X](https://code.google.com/p/git-osx-installer/). It is also
available via Homebrew or MacPorts.

You will also need to install [Homebrew](http://brew.sh)
or [MacPorts](https://www.macports.org/) in order to install library
dependencies. It's largely a religious decision which to choose, but, as of
December 2012, MacPorts is a little easier because you can just install the
dependencies immediately - no other work required. If you're unsure, read
the instructions through first in order to assess what you want to do.
Homebrew is a little more popular among those newer to OS X.

The installation of the actual dependencies is covered in the Instructions
sections below.

Instructions: MacPorts
----------------------

### Install dependencies

Installing the dependencies using MacPorts is very straightforward.

    sudo port install boost db51@+no_java openssl miniupnpc autoconf pkgconfig automake

Optional: install Qt4

    sudo port install qt4-mac qrencode protobuf-cpp

### Building `dogecoind`

1. Clone the github tree to get the source code and go into the directory.

        git clone git@github.com:dogecoin/dogecoin.git dogecoin
        cd dogecoin

2.  Build dogecoind (and Dogecoin-Qt, if configured):

        ./autogen.sh
        ./configure
        make

3.  It is a good idea to build and run the unit tests, too:

        make check

Instructions: Homebrew
----------------------

#### Install dependencies using Homebrew

        brew install autoconf automake berkeley-db boost miniupnpc openssl pkg-config protobuf qt

### Building `dogecoind`

1. Clone the github tree to get the source code and go into the directory.

        git clone https://github.com/dogecoin/dogecoin.git
        cd dogecoin

2.  Build dogecoind:

        ./autogen.sh
        ./configure
        make

3.  It is a good idea to build and run the unit tests, too:

        make check

Creating a release build
------------------------
You can ignore this section if you are building `dogecoind` for your own use.

dogecoind/dogecoin-cli binaries are not included in the Dogecoin-Qt.app bundle.

If you are building `dogecoind` or `Dogecoin-Qt` for others, your build machine should be set up
as follows for maximum compatibility:

All dependencies should be compiled with these flags:

 -mmacosx-version-min=10.6
 -arch x86_64
 -isysroot $(xcode-select --print-path)/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.6.sdk

For MacPorts, that means editing your macports.conf and setting
`macosx_deployment_target` and `build_arch`:

    macosx_deployment_target=10.6
    build_arch=x86_64

... and then uninstalling and re-installing, or simply rebuilding, all ports.

As of December 2012, the `boost` port does not obey `macosx_deployment_target`.
Download `http://gavinandresen-bitcoin.s3.amazonaws.com/boost_macports_fix.zip`
for a fix.

Once dependencies are compiled, see release-process.md for how the Dogecoin-Qt.app
bundle is packaged and signed to create the .dmg disk image that is distributed.

Running
-------

It's now available at `./dogecoind`, provided that you are still in the `src`
directory. We have to first create the RPC configuration file, though.

Run `./dogecoind` to get the filename where it should be put, or just try these
commands:

    echo -e "rpcuser=dogecoinrpc\nrpcpassword=$(xxd -l 16 -p /dev/urandom)" > "/Users/${USER}/Library/Application Support/Dogecoin/dogecoin.conf"
    chmod 600 "/Users/${USER}/Library/Application Support/Dogecoin/dogecoin.conf"

When next you run it, it will start downloading the blockchain, but it won't
output anything while it's doing this. This process may take several hours;
you can monitor its process by looking at the debug.log file, like this:

    tail -f $HOME/Library/Application\ Support/Dogecoin/debug.log

Other commands:

    ./dogecoind -daemon # to start the dogecoin daemon.
    ./dogecoin-cli --help  # for a list of command-line options.
    ./dogecoin-cli help    # When the daemon is running, to get a list of RPC commands

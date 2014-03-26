WINDOWS BUILD NOTES
===================


Compilers Supported
-------------------
TODO: What works?
Note: releases are cross-compiled using mingw running on Linux.


Dependencies
------------
Libraries you need to download separately and build:

	name            default path               download
	--------------------------------------------------------------------------------------------------------------------
	OpenSSL         \openssl-1.0.1c-mgw        http://www.openssl.org/source/
	Berkeley DB     \db-4.8.30.NC-mgw          http://www.oracle.com/technology/software/products/berkeley-db/index.html
	Boost           \boost-1.55.0-mgw          http://www.boost.org/users/download/
	miniupnpc       \miniupnpc-1.6-mgw         http://miniupnp.tuxfamily.org/files/
    qt                                         http://download.qt-project.org/official_releases/qt/5.2/5.2.1/
    protobuf                                   http://protobuf.googlecode.com/files/protobuf-2.5.0.zip
    libpng                                     http://prdownloads.sourceforge.net/libpng/libpng-1.6.9.tar.gz?download
    libqrencode                                http://fukuchi.org/works/qrencode/qrencode-3.4.3.tar.gz

Their licenses:

	OpenSSL        Old BSD license with the problematic advertising requirement
	Berkeley DB    New BSD license with additional requirement that linked software must be free open source
	Boost          MIT-like license
	miniupnpc      New (3-clause) BSD license
    qt             
    protobuf       
    libpng         
    libqrencode    

Versions used in this release:

	OpenSSL      1.0.1c
	Berkeley DB  4.8.30.NC
	Boost        1.55.0
	miniupnpc    1.6
    qt           4.8.3
    protobuf     2.5.0
    libpng       1.6.9
    libqrencode  3.2.0


OpenSSL
-------
MSYS shell:

un-tar sources with MSYS 'tar xfz' to avoid issue with symlinks (OpenSSL ticket 2377)
change 'MAKE' env. variable from 'C:\MinGW32\bin\mingw32-make.exe' to '/c/MinGW32/bin/mingw32-make.exe'

	cd /c/openssl-1.0.1c-mgw
	./config
	make

Berkeley DB
-----------
MSYS shell:

	cd /c/db-4.8.30.NC-mgw/build_unix
	sh ../dist/configure --enable-mingw --enable-cxx
	make

Boost
-----
MS-DOS shell:

	cd boost_1_55_0\
    bootstrap.bat mingw
    b2 --build-type=complete --with-chrono --with-filesystem --with-program_options --with-system --with-thread toolset=gcc stage

MiniUPnPc
---------
UPnP support is optional, make with `USE_UPNP=` to disable it.

MSYS shell:

	cd /c/miniupnpc-1.6-mgw
	make -f Makefile.mingw
	mkdir miniupnpc
	cp *.h miniupnpc/

Dogecoin
-------
MSYS shell:

	cd \dogecoin
	sh autogen.sh
	sh configure
	mingw32-make
	strip dogecoind.exe

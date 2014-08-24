WINDOWS BUILD NOTES
===================


Compilers Supported
-------------------
Compilation under Windows is supported using MinGW (http://www.mingw.org/)


Dependencies
------------
To build Dogecoin for Windows, there are number of libraries you need to download
separately and build. Note that, for those used to a Linux/UNIX background, the search
paths for include files is not the conventional UNIX directories, see
http://www.mingw.org/wiki/IncludePathHOWTO for more details. Library files can be placed
in the more conventional /usr/local/lib directory, but see
http://www.mingw.org/wiki/HOWTO_Specify_the_Location_of_Libraries_for_use_with_MinGW in
case of difficulties.

In both cases, include and library files will need to be placed in the relevant directories
for the build process to succeed.

	name            default path               download
	--------------------------------------------------------------------------------------------------------------------
	OpenSSL         \openssl-1.0.1i-mgw        http://www.openssl.org/source/
	Berkeley DB     \db-5.1.29.NC-mgw          http://www.oracle.com/technology/software/products/berkeley-db/index.html
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

	OpenSSL      1.0.1i
	Berkeley DB  5.1.29.NC
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

	cd /c/openssl-1.0.1i-mgw
	./config
	make

Berkeley DB
-----------
MSYS shell:

	cd /c/db-5.1.29.NC-mgw/build_unix
	sh ../dist/configure --enable-mingw --enable-cxx
	make

Include and library files files should then be placed into MinGW search paths, for example:

Include: C:\MinGW\lib\gcc\mingw32\4.8.1\include\c++
Library: C:\MinGW\lib
    
Boost
-----
MSYS shell:

	cd boost_1_55_0/tools/build/v2/engine
    ./build.sh mingw
    cp bin.ntx86/bjam.exe ../../../../
    cd ../../../../
    bjam --toolset=gcc
    
In case of problems, http://stackoverflow.com/questions/13256788/building-boost-1-52-with-mingw may be useful

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

	dogecoin
	./autogen.sh
	BOOST_ROOT=../boost_1_55_0 ./configure --disable-tests
	mingw32-make
	strip dogecoind.exe

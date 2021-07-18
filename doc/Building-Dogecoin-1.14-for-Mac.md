### Building dogecoin-qt 1.14 on modern macs. ###

Note that this requires changes made by michilumin on the 1.14-dev branch as well as some changes to BDB.

Tested on OSX 10.11 El Capitan, 10.13 High Sierra and 11.1 Big Sur.

**Paper wallet printing support seems to work fine through this method as well.**

### Clone dogecoin locally, or check it out, etc. ###

For this purpose, just indicating the 1.14-branding branch in my repo.

	$git clone -b 1.14-branding --single-branch https://github.com/michilumin/dogecoin.git

### Set up OSX basic build dependencies. ##

Install xcode-select commandline utils.

    $xcode-select --install

**note:** If you have Xcode installed, simply zip it up and move it for this process, as your current Xcode install will likely conflict. Unzip it back later.

Make sure frameworks dir is properly owned...

    $sudo mkdir /usr/local/Frameworks
    $sudo chown $(whoami):admin /usr/local/Frameworks

Install Brew. (If you already have Brew installed, perform a 'brew update'.)

    $/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

Install dependencies via Brew.

    $brew install autoconf automake libtool miniupnpc openssl pkg-config protobuf qt5 zeromq qrencode librsvg boost

Install Boost lib via Brew from source, and link it to be sure:

**note** Boost version may have changed by the time you're reading this, from 167.

    $brew install boost --build-from-source --HEAD
    $brew link boost167

### Compile BDB 5.1 ###
    
    $ make -C depends bdb

### Set some environment variables and links for bdb and openssl ###
    
    $export BDB_PREFIX="/usr/local/BerkeleyDB.5.1"
    $export BDB_LIBS="-L${BDB_PREFIX}/lib -ldb_cxx"
    $export BDB_CFLAGS="-I${BDB_PREFIX}/include"
    
   _**NOTE:** for MacOS BigSur (11.1) or later, and possibly Catalina (10.15) you will also have to include the "OBJC_OLD_DISPATCH_PROTOTYPES=1" flag._
   
   _So in this case you want the above export to be:_
    
    $export CPPFLAGS="-I/usr/local/BerkeleyDB.5.1/include -DOBJC_OLD_DISPATCH_PROTOTYPES=1"
   
   _(Note that the quotes are required.)_
	
    $export INCPATHS=-I/usr/local/opt/openssl/include
    $export LIBPATHS=-L/usr/local/opt/openssl/lib
    $cd /usr/local/include 
    $ln -s ../opt/openssl/include/openssl 

### Go back to your Dogecoin repo ###

    $cd ~/dogecoin
    $./autogen.sh
    $./configure --with-gui=qt5 --with-qrcode=yes
    $make

Go have a beverage.

    $make install

Go have another beverage.

Run it.

	$/usr/local/bin/dogecoin-qt






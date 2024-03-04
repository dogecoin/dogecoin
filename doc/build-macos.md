# Building dogecoin-qt 1.14 on MacOS #

Tested on MacOs Ventura and Sonoma on Intel (x86_64) and Apple Silicon (arm64) macs.

### Clone dogecoin locally, or check it out, etc. ###

```sh
git clone https://github.com/dogecoin/dogecoin.git
```

### Set up OSX basic build dependencies. ##

Install xcode-select commandline utils.

```sh
xcode-select --install
```

Make sure frameworks dir is properly owned...

```sh
sudo mkdir -p /usr/local/Frameworks
sudo chown $(whoami):admin /usr/local/Frameworks
```

Install Brew. (If you already have Brew installed, perform a `brew update`.)

```sh
git clone https://github.com/Homebrew/brew homebrew
eval "$(homebrew/bin/brew shellenv)"
brew update --force --quiet
chmod -R go-w "$(brew --prefix)/share/zsh"
```

Install dependencies via Brew.

```sh
brew install autoconf automake libtool miniupnpc openssl pkg-config protobuf@21 \
             qt5 zeromq qrencode librsvg boost berkeley-db@5 libevent
brew link protobuf@21
```

### Go back to your Dogecoin repo ###

```sh
cd ~/dogecoin

./autogen.sh
./configure --with-gui=qt5 --with-boost=`brew --prefix boost`
make
```

Go have a beverage.

```sh
make install
```

Go have another beverage.

Run it.

```sh
/usr/local/bin/dogecoin-qt
```

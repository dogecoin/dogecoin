#!/bin/bash
# ScrapCoin Installation Script for Ubuntu 24.04+ (Noble and newer)

set -e

echo "=== ScrapCoin Dependencies Installer ==="
echo "This script will install all required dependencies for building ScrapCoin."
echo ""

# Check if running as root
if [[ $EUID -ne 0 ]]; then
   echo "This script must be run as root (use sudo)" 
   exit 1
fi

# Update package list
echo "[1/7] Updating package list..."
apt-get update

# Install basic build tools
echo "[2/7] Installing build essentials..."
apt-get install -y \
    build-essential \
    libtool \
    autotools-dev \
    automake \
    pkg-config \
    bsdmainutils \
    python3 \
    python3-pip \
    libevent-dev \
    libboost-system-dev \
    libboost-filesystem-dev \
    libboost-test-dev \
    libboost-thread-dev \
    libssl-dev \
    libminiupnpc-dev

# Check if Berkeley DB 4.8 is available
echo "[3/7] Checking for Berkeley DB 4.8..."
if apt-cache show libdb4.8-dev &>/dev/null; then
    echo "Berkeley DB 4.8 is available in repositories."
    apt-get install -y libdb4.8-dev libdb4.8++-dev
else
    echo "Berkeley DB 4.8 not found in repositories."
    echo "Building Berkeley DB 4.8 from source..."
    
    # Install wget if not present
    apt-get install -y wget
    
    # Download and build Berkeley DB 4.8
    cd /tmp
    wget -q http://download.oracle.com/berkeley-db/db-4.8.30.NC.tar.gz
    tar -xzf db-4.8.30.NC.tar.gz
    cd db-4.8.30.NC/build_unix
    
    ../dist/configure --enable-cxx --disable-shared --with-pic --prefix=/usr/local
    make -j$(nproc)
    make install
    
    # Create symlinks for compatibility
    ln -sf /usr/local/lib/libdb-4.8.so /usr/local/lib/libdb4.8.so
    ln -sf /usr/local/lib/libdb_cxx-4.8.so /usr/local/lib/libdb4.8++.so
    
    echo "Berkeley DB 4.8 installed to /usr/local"
    
    # Add to ldconfig
    echo "/usr/local/lib" > /etc/ld.so.conf.d/berkeleydb.conf
    ldconfig
    
    cd /tmp
    rm -rf db-4.8.30.NC db-4.8.30.NC.tar.gz
fi

# Verify installations
echo "[4/7] Verifying installations..."
echo -n "Boost version: "
pkg-config --modversion boost 2>/dev/null || echo "Not found (but may be installed)"
echo -n "Berkeley DB version: "
ldconfig -p | grep libdb | head -1 || echo "Not found"

# Optional: Install GUI dependencies
read -p "[5/7] Do you want to build the GUI (scrapcoin-qt)? (y/N): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "Installing Qt dependencies..."
    apt-get install -y \
        libqt5gui5 \
        libqt5core5a \
        libqt5dbus5 \
        libqt5network5 \
        libqt5widgets5 \
        libqt5svg5 \
        qttools5-dev-tools \
        qttools5-dev
fi

# Optional: Install wallet dependencies (if not already done)
echo "[6/7] Wallet support should now be available."
echo "If you encountered errors about libdb, the script attempted to fix it."

# Summary
echo ""
echo "[7/7] Installation complete!"
echo ""
echo "Next steps:"
echo "1. cd /vercel/sandbox/repos/dogecoin"
echo "2. git checkout scrapcoin-fork"
echo "3. ./autogen.sh"
echo "4. ./configure --disable-tests --disable-bench --without-miniupnpc"
echo "5. make -j$(nproc)"
echo ""
echo "If you don't need wallet functionality, use:"
echo "  ./configure --disable-tests --disable-bench --without-miniupnpc --disable-wallet"
echo ""
echo "Binaries will be in src/scrapcoind, src/scrapcoin-cli, etc."
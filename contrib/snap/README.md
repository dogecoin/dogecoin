# Dogecoin Snap Packaging

Commands for building and uploading a Dogecoin Core Snap to the Snap Store. Anyone on amd64 (x86_64), arm64 (aarch64), or i386 (i686) should be able to build it themselves with these instructions. This would pull the official Dogecoin binaries from the releases page, verify them, and install them on a user's machine.

## Building Locally
```
sudo apt install snapd
sudo snap install --classic snapcraft
sudo snapcraft
```

### Installing Locally
```
snap install \*.snap --devmode
```

### To Upload to the Snap Store
```
snapcraft login
snapcraft register dogecoin-core
snapcraft upload \*.snap
sudo snap install dogecoin-core
```

### Usage
```
dogecoin-unofficial.cli # for dogecoin-cli
dogecoin-unofficial.d # for dogecoind
dogecoin-unofficial.qt # for dogecoin-qt
dogecoin-unofficial.test # for test_dogecoin
dogecoin-unofficial.tx # for dogecoin-tx
```

### Uninstalling
```
sudo snap remove dogecoin-unofficial
```
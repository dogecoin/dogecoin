# Pepecoin Snap Packaging

Commands for building and uploading a Pepecoin Core Snap to the Snap Store. Anyone on amd64 (x86_64), arm64 (aarch64), or i386 (i686) should be able to build it themselves with these instructions. This would pull the official Pepecoin binaries from the releases page, verify them, and install them on a user's machine.

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
snapcraft register pepecoin-core
snapcraft upload \*.snap
sudo snap install pepecoin-core
```

### Usage
```
pepecoin-unofficial.cli # for pepecoin-cli
pepecoin-unofficial.d # for pepecoind
pepecoin-unofficial.qt # for pepecoin-qt
pepecoin-unofficial.test # for test_pepecoin
pepecoin-unofficial.tx # for pepecoin-tx
```

### Uninstalling
```
sudo snap remove pepecoin-unofficial
```
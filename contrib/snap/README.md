# ScrapCoin Snap Packaging

Commands for building and uploading a ScrapCoin Core Snap to the Snap Store. Anyone on amd64 (x86_64), arm64 (aarch64), or i386 (i686) should be able to build it themselves with these instructions. This would pull the official ScrapCoin binaries from the releases page, verify them, and install them on a user's machine.

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
snapcraft register scrapcoin-core
snapcraft upload \*.snap
sudo snap install scrapcoin-core
```

### Usage
```
scrapcoin-unofficial.cli # for scrapcoin-cli
scrapcoin-unofficial.d # for scrapcoind
scrapcoin-unofficial.qt # for scrapcoin-qt
scrapcoin-unofficial.test # for test_scrapcoin
scrapcoin-unofficial.tx # for scrapcoin-tx
```

### Uninstalling
```
sudo snap remove scrapcoin-unofficial
```
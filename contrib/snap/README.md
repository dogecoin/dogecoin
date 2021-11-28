Here are all the commands for building and uploading to the Snap Store. They should work for amd64 (x86_64), arm64 (aarch64), and i386 (i686).

---

## Building Locally
```
sudo apt install snapd
sudo snap install --classic snapcraft
snapcraft
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

## Usage
```
dogecoin-core.cli # for dogecoin-cli
dogecoin-core.d # for dogecoind
dogecoin-core.qt # for dogecoin-qt
dogecoin-core.tx # for dogecoin-tx
dogecoin-core.test # for test_dogecoin
```

## Uninstalling
```
sudo snap remove dogecoin-core
```

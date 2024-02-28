# Dogecoin Snap Packaging

Commands for building and uploading a Dogecoin Core Snap to the Snap Store. Anyone on amd64 (x86_64) or arm64 (aarch64) should be able to build it themselves with these instructions. This would pull the official Dogecoin binaries from the releases page, verify them, and install them on a user's machine.

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

### Usage
```
dogecoin-core.cli # for dogecoin-cli
dogecoin-core.d # for dogecoind
dogecoin-core.qt # for dogecoin-qt
dogecoin-core.test # for test_dogecoin
dogecoin-core.tx # for dogecoin-tx
```

## Updating
```
sudo snap refresh dogecoin-core
```
Unless disabled, automatic updates take place whenever there is a new stable release.

## Uninstalling
```
sudo snap remove dogecoin-core
```
To remove all user data, add `--purge`. Be careful, as this will delete your `wallet.dat` file.

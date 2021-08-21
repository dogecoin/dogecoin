All the commands I used for building and pushing to Snapcraft. I've only tested the amd64 .snap package, but anyone on amd64, arm64, or i686 should be able to build and/or install it for themselves with these instructions.

---

## Building Locally

```
sudo apt install snapd
sudo snap install --classic snapcraft
snapcraft
```

### To Install Locally
```
snap install \*.snap --devmode
```

### To Push to Snapcraft
```
snapcraft login
snapcraft register dogecoin-core
snapcraft push \*.snap --release=edge
sudo snap install dogecoin-core --channel=edge
```
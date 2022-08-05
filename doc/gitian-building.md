
# Gitian building

> *Gitian is a secure source-control oriented software distribution method. This means you can download trusted binaries that are verified by multiple builders.*  
Source : [Gitian.org](https://gitian.org/)

Gitian is a deterministic build process that is used to release Dogecoin Core executables. It uses a virtualized environment and a predetermined set of dependencies and system libraries to build binaries. This lets many independent builders compare and publish their results before releasing executables, allowing for an end-to-end auditable release that can be verified by anyone.

To reduce the probability of compromised releases, more independent gitian builders are needed!

Anyone can participate and help to increase the security of Dogecoin Core releases by following this guide.

### Table of contents

1. [Installing dependencies](#installing-dependencies)
    * [Common dependencies](#common-dependencies)
    * [Docker](#docker)
    * [LXC](#lxc)
    * [KVM](#kvm)
2. [Usage](#usage)
    * [Syntax](#syntax)
    * [Example](#example)
    * [Signing externally](#signing-externally)
3. [Publishing signatures](#publishing-signatures)

## Installing dependencies

To perform a gitian build, you can use different virtualization software : Docker, KVM or LXC. Dependencies will change according to your choice.

You need to install some required dependencies whatever you will choose.  

Use your packet manager to install them : `apt`, `brew`, `dnf`, `pacman`...

### Common dependencies

The following common dependencies are required regardless of virtualization:
```
git ruby wget apt-cacher-ng gpg
```

_Note: When asked to allow secure tunnels through apt-cacher, answer "no"_

After installation, you will need to enable `apt-cacher`, eg:

```bash
sudo systemctl enable apt-cacher-ng.service
sudo systemctl start apt-cacher-ng.service
```

You can define your `apt-cacher` host by specifying `MIRROR_HOST` environment variable.

*To create a PGP key to sign files, see : https://gnupg.org/gph/en/manual.html#INTRO.  
You will need to specify your [user ID](https://www.gnupg.org/documentation/manuals/gnupg/Specify-a-User-ID.html), find it using `gpg -k`.*

### Docker

Please refer to the [official Docker documentation](https://docs.docker.com/engine/install/) to install it for your operating system.

Make sure your user can run the `docker` command without root privilege by being in the docker group:
```bash
sudo usermod -aG docker $(whoami)

#Enable group without logging out
newgrp docker
```

You can now use the `--docker` option with `gitian-build.sh` in the [Usage](#usage) section of this guide.

### LXC
Install the following package :
```
lxc
```

Then use `--lxc` option with `gitian-build.sh`.

### KVM

[Documentation not available, help is welcome]

## Usage

`gitian-build.sh` is a standalone script, it can be downloaded and run outside of Dogecoin Core repository.

It can download dependency files for the [Gitian](https://github.com/devrandom/gitian-builder), build and optionally sign binaries, or verify signatures.

Binaries and signatures will be created in a `gitian-output` folder, relative to where the
`gitian-build.sh` script is ran.

### Syntax

```bash
./gitian-build.sh [options] version

#See help menu for available options
./gitian-build.sh --help
```

### Example

The entire gitian flow can be performed step by step, example using docker :
```bash
#Download Gitian dependencies
./gitian-build.sh --docker --setup 1.14.5

#Build & sign executables
./gitian-build.sh --docker --build --sign SIGNER 1.14.5

#Verify signatures
./gitian-build.sh --verify 1.14.5
```

Or to do everything at once :
```bash
./gitian-build.sh --docker --setup --build --sign SIGNER --verify 1.14.5
```

### Signing externally

If you want to do the PGP signing on another device, that's also possible; just define `SIGNER` as mentioned
and follow the steps in the build process as normal.

```bash
gpg: skipped "shibetoshi": secret key not available
```

When you execute `gsign` you will get an error from GPG, which can be ignored. Copy the resulting `.assert` files in `gitian.sigs` to your signing machine and do

```bash
gpg --detach-sign ${VERSION}-linux/${SIGNER}/dogecoin-linux-build.assert
gpg --detach-sign ${VERSION}-win/${SIGNER}/dogecoin-win-build.assert
gpg --detach-sign ${VERSION}-osx-unsigned/${SIGNER}/dogecoin-osx-build.assert
```

This will create the `.sig` files that can be committed together with the `.assert` files to assert your Gitian build.

## Publish signatures

Gitian signatures for each release are added to https://github.com/dogecoin/gitian.sigs.

`gitian-build.sh` will create signatures inside `gitian-output/sigs/` folder. Create a pull request to [dogecoin/gitian.sigs](https://github.com/dogecoin/gitian.sigs) to publish your signatures, the `.assert` and `.assert.sig` files.

**When your PR is merged, you will be recorded for all future history as a *Gitian Builder of Dogecoin Core*!**

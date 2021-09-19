
# Gitian building

> *Gitian is a secure source-control oriented software distribution method. This means you can download trusted binaries that are verified by multiple builders.*  
Source : [Gitian.org](https://gitian.org/)

Gitian is a deterministic build process that is used to release Dogecoin Core executables. It use same dependencies in a virtualized environment to build binaries. This let independent builders compare binaries hashes to verify and sign released executables.

To reduce probability of compromised binaries during releases, more independent gitian builders are needed !  

Participate and help to secure the process by following this guide.

### Table of contents

1. [Install dependencies](#install-dependencies)
    * [Common dependencies](#common-dependencies)
    * [Docker](#docker)
    * [LXC](#lxc)
    * [KVM](#kvm)
    * [Apt-cacher](#apt-cacher)
2. [Usage](#usage)
    * [Syntax](#syntax)
    * [Example](#example)
    * [Signing externally](#signing-externally)
3. [Publish signatures](#publish-signatures)

## Install dependencies

To perform a gitian build, you can use different virtualization software : Docker, KVM or LXC. Dependencies will change according to your choice.

You need to install some required dependencies whatever you will choose.  

Use your packet manager to install them : `apt`, `brew`, `dnf`, `pacman`...

### Common dependencies

Following dependencies are required to run `gitian-build.sh`:
```
git ruby wget
```

Optionally, to sign or verify binaries :
```
pgp
```
*To create a PGP key to sign files, see : https://gnupg.org/gph/en/manual.html#INTRO.  
You will need to specify your [user ID](https://www.gnupg.org/documentation/manuals/gnupg/Specify-a-User-ID.html), find it using `gpg -k`.*

### Docker

Follow [Docker official documentation](https://docs.docker.com/engine/install/) to install it for your operating system.

Make sure your user can run `docker` command without root privilege by being in the docker group :
```bash
$ sudo usermod -aG docker $(whoami)

# Enable group without logging out
$ newgrp docker
```

Then use `--docker` option with `gitian-build.sh`.

### LXC
Install the following package :
```
lxc
```

Then use `--lxc` option with `gitian-build.sh`.

### KVM

[Documentation not available, help is welcome]

### Apt-cacher

Disabled by default, `apt-cacher` enable to cache locally downloaded dependencies to save resources.

You will need the following package :
```
apache2 apt-cacher-ng
```

You can use your local server by using `--enable-apt-cacher`, or define `MIRROR_HOST` if you need to specify the server address.

> **Be nice:** Please use cache for intensive ressource usage to save mirrors bandwith.

## Usage

`gitian-build.sh` is a standalone script, it can be downloaded and run outside of Dogecoin Core repository.

It can download dependency files for the [Gitian](https://github.com/devrandom/gitian-builder), build and optionally sign binaries, or verify signatures.

Binaries and signatures will be created in folder `gitian-output/`.

### Syntax

```bash
$ ./gitian-build.sh [options] version

# See help menu for available options
$ ./gitian-build.sh --help
Arguments:
version             Version number, commit, or branch to build. If building a
                    commit or branch, the -c option must be specified

Options:
--setup             Setup dependencies for the gitian building environment. Uses Docker
-b|--build          Do the gitian build
-B|--build-signed   Build signed binaries for MacOS and Windows
-s|--sign name      Sign builded executables with GPG
-v|--verify         Verify the gitian build
--lxc               Use LXC instead of KVM
--docker            Use Docker instead of KVM
-o|--os lwx         Specify which Operating Systems the build is for. Default is lwx,
                    l for Linux, w for Windows, x for MacOS
-j proc             Number of processes to use. Default 2
-m n                Memory to allocate in MiB. Default 2000
--enable-cache      Use local apt-cacher server. If you need to specify host, use
                    MIRROR_HOST environment variable

-c|--commit         Indicate that the version argument is for a commit or branch
-u|--url repo       Specify the URL of the repository. Default is https://github.com/dogecoin/dogecoin
--test              CI TEST. Uses Docker
-h|--help           Print this help message
```

### Example

The entire gitian flow can be performed step by step, example using docker :
```bash
# Download Gitian dependencies
$ ./gitian-build.sh --docker --setup 1.14.4

# Build & sign executables
$ ./gitian-build.sh --docker --build --sign SIGNER 1.14.4

# Verify signatures
$ ./gitian-build.sh --verify 1.14.4
```

Or to do everything at once :
```bash
$ ./gitian-build.sh --docker --setup --build --sign SIGNER --verify 1.14.4
```

### Signing externally

If you want to do the PGP signing on another device, that's also possible; just define `SIGNER` as mentioned
and follow the steps in the build process as normal.

```bash
$ gpg: skipped "laanwj": secret key not available
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

**When your PR is merged, you will officially be a *Gitian Builder of Dogecoin* !**


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
You can define your `apt-cacher` host by specifying `MIRROR_HOST` environment variable.

*To create a PGP key to sign files, see : https://gnupg.org/gph/en/manual.html#INTRO.  
You will need to specify your [user ID](https://www.gnupg.org/documentation/manuals/gnupg/Specify-a-User-ID.html), find it using `gpg -k`.*

### Docker

Please refer to the [official Docker documentation](https://docs.docker.com/engine/install/) to install it for your operating system.

Make sure your user can run the `docker` command without root privilege by being in the docker group:
```console
shibetoshi:~$ sudo usermod -aG docker $(whoami)

# Enable group without logging out
shibetoshi:~$ newgrp docker
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

```console
shibetoshi:~$ ./gitian-build.sh [options] version

# See help menu for available options
shibetoshi:~$ ./gitian-build.sh --help
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
```console
# Download Gitian dependencies
shibetoshi:~$ ./gitian-build.sh --docker --setup 1.14.4

# Build & sign executables
shibetoshi:~$ ./gitian-build.sh --docker --build --sign SIGNER 1.14.4

# Verify signatures
shibetoshi:~$ ./gitian-build.sh --verify 1.14.4
```

Or to do everything at once :
```console
shibetoshi:~$ ./gitian-build.sh --docker --setup --build --sign SIGNER --verify 1.14.4
```

### Signing externally

If you want to do the PGP signing on another device, that's also possible; just define `SIGNER` as mentioned
and follow the steps in the build process as normal.

```console
shibetoshi:~$ gpg: skipped "shibetoshi": secret key not available
```

When you execute `gsign` you will get an error from GPG, which can be ignored. Copy the resulting `.assert` files in `gitian.sigs` to your signing machine and do

```console
shibetoshi:~$ gpg --detach-sign ${VERSION}-linux/${SIGNER}/dogecoin-linux-build.assert
shibetoshi:~$ gpg --detach-sign ${VERSION}-win/${SIGNER}/dogecoin-win-build.assert
shibetoshi:~$ gpg --detach-sign ${VERSION}-osx-unsigned/${SIGNER}/dogecoin-osx-build.assert
```

This will create the `.sig` files that can be committed together with the `.assert` files to assert your Gitian build.

## Publish signatures

Gitian signatures for each release are added to https://github.com/dogecoin/gitian.sigs.

`gitian-build.sh` will create signatures inside `gitian-output/sigs/` folder. Create a pull request to [dogecoin/gitian.sigs](https://github.com/dogecoin/gitian.sigs) to publish your signatures, the `.assert` and `.assert.sig` files.

**When your PR is merged, you will be recorded for all future history as a *Gitian Builder of Dogecoin Core*!**

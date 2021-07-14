<h1 align="center">
Dogecoin Core [DOGE, Ð]  
<br/><br/>
<img src="https://static.tumblr.com/ppdj5y9/Ae9mxmxtp/300coin.png" alt="Dogecoin" width="300"/>
</h1>

# Gitian building

Setup instructions for a Gitian build of Dogecoin Core using a Docker. https://github.com/micaelmalta/gitian-builder-dogecoin

Gitian is the deterministic build process that is used to build the Dogecoin Core executables. It provides a way to be reasonably sure that the executables are really built from the source on GitHub. It also makes sure that the same, tested dependencies are used and statically built into the executable.

Multiple developers build the source code by following a specific descriptor ("recipe"), cryptographically sign the result, and upload the resulting signature. These results are compared and only if they match, the build is accepted and uploaded to dogecoin.com.

More independent Gitian builders are needed, which is why this guide exists. It is preferred you follow these steps yourself instead of using someone else's VM image to avoid 'contaminating' the build.


# SUPPORTED DISTRIBUTION

    ubuntu
    debian
    centos
    fedora
    archlinux
    macos

# PREREQUISITE

## MANUAL INSTALL
    1. Install Docker

        https://docs.docker.com/engine/install/

    2. Generate GPG key

        https://docs.github.com/en/github/authenticating-to-github/managing-commit-signature-verification/generating-a-new-gpg-key

    3. INSTALL those packages:

        sudo git make wget brew(MACOS ONLY)

    
## SETUP  (ONLY ONCE)
    ./gitian-build.sh --setup

## BUILD
    ./gitian-build.sh -j <jobs> -m <mem> --build <signer_uid:required> <version:required>

## VERIFY
    ./gitian-build.sh --verify <signer_uid:required> <version:required>

## VERSION vs COMMIT
  For release version: omit `v` 

    ./gitian-build.sh -j <jobs> -m <mem> --build <signer_uid:required> 1.14.3

  For commit or branch: use `--commit`
    
    ./gitian-build.sh -j <jobs> -m <mem> --commit --build <signer_uid:required> <branch|hash>

## PUSH SIGN TO gitian.sigs
    ./gitian-build.sh --push

## CUSTOM REPOSITORY
    ./gitian-build.sh -j <jobs> -m <mem> --build -u <repo_url> <signer_uid:required> <version:required>

## COMPLETE LIST OF PARAMETERS
    ./gitian-build.sh --help

## Examples:
    ./gitian-build.sh --setup
    
    ./gitian-build.sh -j 8 -m 8192 -u https://github.com/dogecoin/dogecoin --build signer 1.14.3

    ./gitian-build.sh -j 8 -m 8192 --build signer 1.14.3

    or

    ./gitian-build.sh -j 8 -m 8192 --commit --build signer 1.14.4-dev

    ./gitian-build.sh --push

## TROUBLESHOOT
    
### DOCKER ERROR

    Got permission denied while trying to connect to the Docker daemon socket at unix:///var/run/docker.sock

Make sure your user in on Docker group

    sudo usermod -aG docker `whoami`
    newgrp docker


### GPG SIGNER UID

In order to sign the builds, you need to know your uid. To do it, follow the GNUGPG documentation

https://www.gnupg.org/documentation/manuals/gnupg/Specify-a-User-ID.html

You can also launch this command on your terminal to have the list

    gpg --list-keys | grep uid | grep ultimate


## License

Dogecoin Core is released under the terms of the MIT license. See COPYING for more information or see https://opensource.org/licenses/MIT.

## OTHER CREDITS

https://github.com/patricklodder

https://gist.github.com/patricklodder/88d6c4e3406db840963f85d95ceb44fe
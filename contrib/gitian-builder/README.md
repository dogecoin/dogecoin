<h1 align="center">
Dogecoin Core [DOGE, Ð]  
<br/><br/>
<img src="https://static.tumblr.com/ppdj5y9/Ae9mxmxtp/300coin.png" alt="Dogecoin" width="300"/>
</h1>

# Gitian building

Setup instructions for a Gitian build of Dogecoin Core using a Docker.

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
## AUTOMATED INSTALL
    1. Launch automated script (ONLY ONCE)

        ./gitian-build.sh --init

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
    ./gitian-build.sh -j <jobs> -m <mem> --build <signer_name:required> <version:required>

## VERIFY
    ./gitian-build.sh --verify <signer_name:required> <version:required>

## VERSION vs COMMIT
  For release version: omit `v` 

    ./gitian-build.sh -j <jobs> -m <mem> --build <signer_name:required> 1.14.3

  For commit or branch: use `--commit`
    
    ./gitian-build.sh -j <jobs> -m <mem> --commit --build <signer_name:required> <branch|hash>

## PUSH SIGN TO gitian.sigs
    ./gitian-build.sh --push

## CUSTOM REPOSITORY
    ./gitian-build.sh -j <jobs> -m <mem> --build -u <repo_url> <signer_name:required> <version:required>

## COMPLETE LIST OF PARAMETERS
    ./gitian-build.sh --help

## Examples:
    ./gitian-build.sh --setup
    
    ./gitian-build.sh -j 8 -m 8192 -u https://github.com/micaelmalta/dogecoin --build mmicael 1.14.3

    ./gitian-build.sh -j 8 -m 8192 --build mmicael 1.14.3

    or

    ./gitian-build.sh -j 8 -m 8192 --commit --build mmicael 1.14.4-dev

    ./gitian-build.sh --push

## CI TESTING

The purpose of this is to be able to check that builds work with GITIAN using CIRRUS: https://github.com/marketplace/cirrus-ci

### HOW TO DO A GITIAN BUILD

    1/ Fork repo: https://github.com/micaelmalta/gitian-builder-dogecoin
    2/ git clone <your_repo>
    3/ git checkout -b build_<whatever> # IMPORTANT PART AS GITIAN BUILD ARE ONLY BUILD ON build_* branches
    4/ vim .cirrus.yml
    5/ Change those 2 values:
        URL: https://github.com/mmicael/dogecoin
        BRANCH: 1.14.4-libevent
    6/ git commit -m "whatever"
    7/ git push --set-upstream origin <your_branch>
    8/ Wait 45mn!
    9/ ENJOY!

## TROUBLESHOOT
    
### DOCKER ERROR

    Got permission denied while trying to connect to the Docker daemon socket at unix:///var/run/docker.sock

Make sure your user in on Docker group

    sudo usermod -aG docker `whoami`
    newgrp docker


## License

Dogecoin Core is released under the terms of the MIT license. See COPYING for more information or see https://opensource.org/licenses/MIT.

## OTHER CREDITS

https://github.com/patricklodder

https://gist.github.com/patricklodder/88d6c4e3406db840963f85d95ceb44fe
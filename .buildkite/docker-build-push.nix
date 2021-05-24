# This script will load nix-built docker image of cardano-node applications
# into the Docker daemon (must be running), and then push to the Docker Hub.
# Credentials for the hub must already be installed with # "docker login".
#
# There is a little bit of bash logic to replace the default repo and
# tag from the nix-build (../nix/docker.nix).

{ nodePackages ?  import ../. {}

# Build system's Nixpkgs. We use this so that we have the same docker
# version as the docker daemon.
, hostPkgs ? import <nixpkgs> {}

# Dockerhub repository for image tagging.
, dockerHubRepoName ? null
}:

with hostPkgs;
with hostPkgs.lib;

let
  image = impureCreated nodePackages.dockerImage;

  # Override Docker image, setting its creation date to the current time rather than the unix epoch.
  impureCreated = image: image.overrideAttrs (oldAttrs: { created = "now"; }) // { inherit (image) version; };

in
  writeScript "docker-build-push" (''
    #!${runtimeShell}

    set -euo pipefail

    export PATH=${lib.makeBinPath [ docker gnused ]}

    ${if dockerHubRepoName == null then ''
    reponame=cardano-node
    username="$(docker info | sed '/Username:/!d;s/.* //')"
    fullrepo="$username/$reponame"
    '' else ''
    fullrepo="${dockerHubRepoName}"
    ''}

  '' + concatMapStringsSep "\n" (image: ''
    branch="''${BUILDKITE_BRANCH:-}"
    event="''${GITHUB_EVENT_NAME:-}"

    gitrev="${image.imageTag}"

    echo "Loading $fullrepo:$gitrev"
    if [ ! -z "$event" ]
    then
      echo "Received GH Workflow event: $event"
    fi
    docker load -i ${image}

    # If a release event, apply two tags to the image
    # e.g. "1.0.0" AND "latest"
    if [[ "$event" = release ]]; then
      ref="''${GITHUB_REF:-}"
      version="$(echo $ref | sed -e 's/refs\/tags\///')"

      echo "Tagging with a version number: $fullrepo:$version"
      docker tag $fullrepo:$gitrev $fullrepo:$version
      echo "Pushing $fullrepo:$version"
      docker push "$fullrepo:$version"

      echo "Tagging as latest"
      docker tag $fullrepo:$version $fullrepo:latest
      echo "Pushing $fullrepo:latest"
      docker push "$fullrepo:latest"

    # GitHub workflows trigger on pushing a tag
    elif [[ "$event" = push ]]; then
      ref="''${GITHUB_REF:-}"
      version="$(echo $ref | sed -e 's/refs\/tags\///')"

      echo "Tagging with a version number: $fullrepo:$version"
      docker tag $fullrepo:$gitrev $fullrepo:$version
      echo "Pushing $fullrepo:$version"
      docker push "$fullrepo:$version"

    # Every commit to master needs to be tagged with master
    elif [[ "$branch" = master ]]; then
      echo "Tagging as master"
      docker tag $fullrepo:$gitrev $fullrepo:$branch
      echo "Pushing $fullrepo:$branch"
      docker push "$fullrepo:$branch"
    fi

    echo "Cleaning up with docker system prune"
    docker system prune -f
  '') [ image ] )

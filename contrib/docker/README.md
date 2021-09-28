## Docker

Docker can be used to run a node and enable containerization abilities.
The following propose a base image to deploy a Dogecoin Core node.

> **Warning:** Do not use for production.

### Table of content

1. [Install Docker](#install-docker)
2. [How to use this image](#how-to-use-this-image)
3. [Docker run syntax](#docker-run-syntax)
4. [Docker-compose example](#docker-compose-example)

### Install Docker

Follow official [Get Docker](https://docs.docker.com/get-docker/) instructions to install Docker for your own system.

For security reasons, to avoid running `docker` command with `sudo`, add your user in the `docker` group. See [Manage Docker as a non-root user](https://docs.docker.com/engine/install/linux-postinstall/).

### How to use this image

> **Warning:** Even if it's optional, mapping of the data directory with a volume is highly recommended, to avoid redownloading the blockchain at each container creation.

Example to launch a node and start using `dogecoin-cli` :
```bash
# With the repository, go to Dockerfile & docker-entrypoint.py location
$ cd contrib/docker

# Alternatively, download Dockerfile & entrypoint script
$ wget https://raw.githubusercontent.com/dogecoin/dogecoin/master/contrib/docker/docker-entrypoint.py
$ wget https://raw.githubusercontent.com/dogecoin/dogecoin/master/contrib/docker/Dockerfile

# Build the image & start a node
# Use `--build-arg VERSION=x.y.z` to specify a version
$ docker build -t dogecoin .

# Launch your container
$ docker run --name doge-container -v $(pwd)/data:/dogecoin/.dogecoin dogecoin

# Use dogecoin-cli within the container
$ docker exec doge-container dogecoin-cli help
```
If the help command of `dogecoin-cli` show you a list of command, your node is ready !

Alternative example to run a node on testnet, to use some configuration :
```bash
$ docker run -e TESTNET= -e RPCUSER=Shibetoshi dogecoin -rpcpassword=Nakamoto
```
*Environment variables and command line arguments are both available.*

#### Docker run syntax

```
docker run  [docker-options] image [dogecoin-executable] [executable-arguments]
```
`docker-options` : Set environment variables, ports, volumes and other docker settings.  
`image` : Image name, built from Dockerfile.  
`dogecoin-executable` : Choose between `dogecoind`, `dogecoin-cli`, `dogecoin-tx` or `dogecoin-qt`. Default is `dogecoind`.  
`executable-arguments` : Pass arguments directly to the selected executable.

Node configuration can be provided by using `dogecoin.conf` within a volume, directly as arguments or/and as environment variables.

#### Docker-compose example

To facilitate configuration of containers, you can also choose to use [docker-compose](https://docs.docker.com/compose/install/).

Example of a basic `docker-compose.yml` :
```yaml
services:
  dogecoin:
    # Need to match Dockerfile & docker-entrypoint.py location
    build: ./contrib/docker
    ports:
      - "44555:44555"
      - "44556:44556"
    volumes:
      - ./data:/dogecoin/.dogecoin
    environment:
      - TESTNET=
      - RPCUSER=Shibetoshi
      - RPCPASSWORD=Nakamoto
    command: -maxconnections=25
```

Then, you can use `docker-compose up` to launch your node.

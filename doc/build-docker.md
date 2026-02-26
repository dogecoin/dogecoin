# Building with Docker

### Building Docker Image

```bash
$ cd /dogecoin
$ docker build -t <repository> .
```

### Running Container with Native Display Sharing

##### Enable Host

```bash
$ xhost +local:docker
```

##### Run Container with Native Display

```bash
$ QT_GRAPHICSSYSTEM="native" docker run -it -e DISPLAY=$DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix <imageId>
```

<u>Note:</u> executing last command will connect to the container automatically. At other times, you can connect via SSH to a Docker container with

```bash
$ docker exec -it <containerId> /bin/bash
```

### Using Dogecoin Core

##### Dogecoin Core Interface
```bash
$ cd /dogecoin/src/qt
$ ./dogecoin-qt
```

Opening the Dogecoin interface for the first time will sync with the blockchain, taking multiple hours to do so. Closing and opening the client will pause and resume synchronisation.

##### dogecoin-cli


```bash
$ cd /dogecoin/src
$ ./dogecoin-cli help
```

##### Dogecoin Daemon


```bash
$ cd /dogecoin/src
$ ./dogecoind -daemon
```

##### 

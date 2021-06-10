FROM ubuntu:latest
RUN apt-get update
# Build deps
RUN DEBIAN_FRONTEND="noninteractive" apt-get install -y build-essential libtool autotools-dev automake pkg-config bsdmainutils python3 curl libfontconfig libxcb1 doxygen
# Dogecoin Core
COPY . /dogecoin
WORKDIR /dogecoin
RUN     cd depends && make HOST=x86_64-linux-gnu && cd .. &&\
        ./autogen.sh && \
        ./configure --prefix=`pwd`/depends/x86_64-linux-gnu --with-gui && \
        make && \
        make install 

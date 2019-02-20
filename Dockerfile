FROM ubuntu:14.04

MAINTAINER Alex Foster version: 0.1 dogerized by RaptorJesus

ENV DOGEVERSION=1.10.0

ENV DOGEPREFIX=/dogecoin/depends/x86_64-pc-linux-gnu

RUN apt-get update && apt-get install -y git build-essential wget pkg-config curl libtool autotools-dev automake libssl-dev libevent-dev bsdmainutils libboost-system-dev libboost-filesystem-dev libboost-chrono-dev libboost-program-options-dev libboost-test-dev libboost-thread-dev

WORKDIR /

RUN mkdir -p /berkeleydb && git clone https://github.com/dogecoin/dogecoin

WORKDIR /berkeleydb

RUN wget http://download.oracle.com/berkeley-db/db-5.1.29.NC.tar.gz && tar -xvf db-5.1.29.NC.tar.gz && rm db-5.1.29.NC.tar.gz && mkdir -p db-5.1.29.NC/build_unix/build

ENV BDB_PREFIX=/berkeleydb/db-5.1.29.NC/build_unix/build

WORKDIR /berkeleydb/db-5.1.29.NC/build_unix

RUN ../dist/configure --disable-shared --enable-cxx --with-pic --prefix=$BDB_PREFIX

RUN make install

RUN apt-get update && apt-get install -y libminiupnpc-dev libqt4-dev libprotobuf-dev protobuf-compiler libqrencode-dev

WORKDIR /dogecoin

RUN git checkout v${DOGEVERSION} && mkdir -p /dogecoin/dogecoin-${DOGEVERSION}

WORKDIR /dogecoin/depends

RUN make

WORKDIR /dogecoin

RUN ./autogen.sh

RUN ./configure CPPFLAGS="-I${BDB_PREFIX}/include/ -O2" LDFLAGS="-L${BDB_PREFIX}/lib/ -static-libstdc++" --with-gui --prefix=${DOGEPREFIX} --disable-ccache --disable-maintainer-mode --disable-dependency-tracking --enable-glibc-back-compat --enable-reduce-exports --disable-bench --disable-gui-tests --enable-static

RUN make 

RUN make install DESTDIR=/dogecoin/dogecoin-${DOGEVERSION}

RUN mv /dogecoin/dogecoin-${DOGEVERSION}${DOGEPREFIX} /dogecoin-${DOGEVERSION} && strip /dogecoin-${DOGEVERSION}/bin/* && rm -rf /dogecoin-${DOGEVERSION}/lib/pkgconfig && find /dogecoin-${DOGEVERSION} -name "lib*.la" -delete && find /dogecoin-${DOGEVERSION} -name "lib*.a" -delete 

WORKDIR /

RUN tar cvf dogecoin-${DOGEVERSION}.tar dogecoin-${DOGEVERSION} 

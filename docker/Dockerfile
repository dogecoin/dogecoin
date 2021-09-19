FROM debian:latest
ARG version
RUN apt update && apt upgrade -y && apt install -y curl gpg ca-certificates tar dirmngr
RUN curl -o dogecoin.tar.gz -Lk https://github.com/dogecoin/dogecoin/releases/download/v${version}/dogecoin-${version}-x86_64-linux-gnu.tar.gz
RUN tar -xvf dogecoin.tar.gz
RUN rm dogecoin.tar.gz
RUN install -m 0755 -o root -g root -t /usr/local/bin dogecoin-${version}/bin/*
RUN mkdir ~/.dogecoin && echo rpcuser=dogecoinrpc > ~/.dogecoin/dogecoin.conf && PWord=`cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 64 | head -n 1` && echo rpcpassword=$PWord >> ~/.dogecoin/dogecoin.conf
EXPOSE 22556
CMD ["dogecoind", "-printtoconsole"]
LABEL name="dogecoin-node" version="${version}" description="Dogecoin fullnode container based off Debian"
LABEL maintainer="Dave Sharone <radardave1@gmai.....>"
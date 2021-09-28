FROM ubuntu:bionic

ARG VERSION=1.14.4
ARG VERSION_HASH=6266235abe4bcbd41ea57bdf42f11ef89aa69f0386e8c8846d5228af69e7fa13

ENV USER=dogecoin
ENV DATADIR=/${USER}/.dogecoin

# Root configuration to mimic user
ENV HOME=/${USER}

RUN useradd ${USER} --home-dir ${HOME}

# Dependencies install
RUN apt update && apt install -y \
    man \
    python3 \
    wget \
    && rm -rf /var/lib/apt/lists/*

# Download Dogecoin Core from github releases.
# Move downloaded binaries and man pages in the container system.
# Setuid on binaries with $USER rights, to limit root usage.
#
# Security: more secure way than check hash for download,
# see https://github.com/docker-library/official-images#security
RUN cd /tmp && \
    wget https://github.com/dogecoin/dogecoin/releases/download/v${VERSION}/dogecoin-${VERSION}-x86_64-linux-gnu.tar.gz && \
    echo "${VERSION_HASH} dogecoin-${VERSION}-x86_64-linux-gnu.tar.gz" | sha256sum -c && \
    tar -xvf dogecoin-${VERSION}-x86_64-linux-gnu.tar.gz --strip-components=1 && \
    cp share/man/man1/*.1 /usr/share/man/man1 && \
    cp bin/dogecoin* /usr/local/bin && \
    chown ${USER}:${USER} /usr/local/bin/dogecoin* && \
    chmod 4555 /usr/local/bin/dogecoin* && \
    rm -rf /tmp/*

COPY docker-entrypoint.py /usr/local/bin/docker-entrypoint

WORKDIR /${USER}

# P2P network (mainnet, testnet & regnet respectively)
EXPOSE 22556 44556 18444

# RPC interface (mainnet, testnet & regnet respectively)
EXPOSE 22555 44555 18332

VOLUME ["/dogecoin/.dogecoin"]

ENTRYPOINT ["docker-entrypoint"]

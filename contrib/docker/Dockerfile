FROM ubuntu:bionic

ARG VERSION=1.14.5
ENV USER=dogecoin
ENV DATADIR=/${USER}/.dogecoin

# User configuration
WORKDIR /${USER}
RUN adduser ${USER} --home /${USER} --disabled-password --gecos ""
# Root configuration to mimic user
ENV HOME=/${USER}

# Dependencies install
RUN apt update
RUN apt install -y wget gosu python3 man

# Download Dogecoin Core from github releases (https://github.com/dogecoin/dogecoin/releases)
# Move downloaded binaries and man path in the system
# Setuid on binaries with $USER rights, to limit root usage.
RUN cd /tmp && \
    wget https://github.com/dogecoin/dogecoin/releases/download/v${VERSION}/dogecoin-${VERSION}-x86_64-linux-gnu.tar.gz && \
    tar -xvf dogecoin-${VERSION}-x86_64-linux-gnu.tar.gz --strip-components=1 && \
    cp share/man/man1/*.1 /usr/share/man/man1 && \
    cp bin/dogecoin* /usr/local/bin && \
    chown ${USER}:${USER} /usr/local/bin/dogecoin* && \
    chmod 4555 /usr/local/bin/dogecoin* && \
    rm -rf /tmp/*

COPY docker-entrypoint.py /entrypoint.py

ENTRYPOINT ["python3", "/entrypoint.py"]

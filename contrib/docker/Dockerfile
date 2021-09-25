FROM debian:latest

ARG VERSION=1.14.5
ENV USER=dogecoin
ENV DATADIR=/${USER}/.dogecoin

# Update root env, to behave like user when using executables
ENV PATH=$PATH:/${USER}/bin
ENV HOME=/${USER}

RUN apt update

# Install dogecoin from github releases (https://github.com/dogecoin/dogecoin/releases)
RUN apt install -y wget gosu python3 man

RUN wget https://github.com/dogecoin/dogecoin/releases/download/v${VERSION}/dogecoin-${VERSION}-x86_64-linux-gnu.tar.gz && \
    mkdir ${USER} && \
    tar -xvf dogecoin-${VERSION}-x86_64-linux-gnu.tar.gz -C ${USER}  --strip-components=1
    
# User configuration
WORKDIR /${USER}

RUN adduser ${USER} --home /${USER} --disabled-password --gecos "" && \
    chown -R ${USER}:${USER} .
    
COPY docker-entrypoint.py /entrypoint.py

ENTRYPOINT ["python3", "/entrypoint.py"]

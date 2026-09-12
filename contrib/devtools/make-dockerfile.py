#!/usr/bin/env python
# Copyright (c) 2023 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

'''
This writes a Dockerfile you can use to test a PR.

Author: @chromatic
'''

import click
from textwrap import dedent

@click.command()
@click.option("--baseimage", default="ubuntu:jammy", help="Base image to use for testing")
@click.option("--remote", default="dogecoin", help="Optional repository to add as a remote")
@click.option("--pr", default="", help="Optional PR to pull")
def main(baseimage, remote, pr):
    dockerfile = f"""
    FROM {baseimage}

    # tz setting to prevent interactive warnings
    ENV TZ=Etc/UTC
    RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

    #### BUILD SYSTEM
    RUN apt-get update && \
        apt-get install -y --no-install-recommends build-essential libtool autotools-dev automake \
          pkg-config libssl-dev libevent-dev bsdmainutils libboost-system-dev \
          libboost-filesystem-dev libboost-chrono-dev \
          libboost-program-options-dev libboost-test-dev \
          libboost-thread-dev libdb5.3++-dev libdb5.3++ \
          libdb5.3-dev libzmq3-dev libminiupnpc-dev libqt5gui5 libqt5core5a \
          libqt5dbus5 qttools5-dev qttools5-dev-tools libprotobuf-dev \
          protobuf-compiler libqrencode-dev git ca-certificates build-essential

    RUN git clone https://github.com/dogecoin/dogecoin.git

    WORKDIR /dogecoin

    RUN git remote add {remote} https://github.com/{remote}/dogecoin.git && \\
        git fetch --all && \\
        git fetch origin pull/{pr}/head:pr{pr} && \\
        git checkout pr{pr}

    RUN ./autogen.sh && \\
        ./configure && \\
        ./make && \\
        ./make check
    """

    print(dedent(dockerfile))

if __name__ == "__main__":
    main()

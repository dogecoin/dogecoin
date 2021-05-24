### Start your nodes

Start a __relay node__ with:

    cardano-node run \
    --topology mainnet-topology.json \
    --database-path /db \
    --socket-path /db/node.socket \
    --host-addr <PUBLIC IPv4 ADDRESS> \
    --port <PORT> \
    --config mainnet-config.json

Note: Use `--host-ipv6-addr` for IPv6 address.

Start a __block producing__ node with:

    cardano-node run \
    --topology mainnet-topology.json \
    --database-path /db \
    --socket-path /db/node.socket \
    --host-addr <PUBLIC IP> \
    --port <PORT> \
    --config mainnet-config.json \
    --shelley-kes-key kes.skey \
    --shelley-vrf-key vrf.skey \
    --shelley-operational-certificate node.cert

Please note that when running a node, it is important to use process monitoring so that the node can be automatically restarted when it terminates unexpectedly. The node relies on being restarted as part of its mechanism for automatically recovering from disk file corruption or other unexpected conditions. In case the node process is not automatically restarted, the node will not be available. Thus for example, stake pools will not be able to create blocks and exchanges will not be able to get wallet information.

We recommend that node operators run their nodes using a standard service monitoring or supervisor tool which will automatically restart the node in case of failure, thereby avoiding downtime. We recommend [systemd](https://www.freedesktop.org/wiki/Software/systemd/) on Linux, [docker’s](https://www.docker.com/) auto-restart functionality, or any equivalent process monitoring tool. This does not apply to Daedalus users, as the node process monitoring is handled by Daedalus itself.

# Dogecoin Core 1.8.3

Dogecoin Core 1.8.3 is a security release. It fixes issues in regards to an upcoming announcement of possible DoS vulnerabilities.
See [the announcement on the bitcoin-dev mailing list](https://lists.linuxfoundation.org/pipermail/bitcoin-dev/2015-June/009135.html) for reference.
It is strongly recommended to upgrade your installation in case you run a node open to inbound connections from the internet.  
If your installation is not open to incoming connections this update is not mandatory, but we still recommend keeping your client up to date.

## Dogecoin Core Release Notes

* Pull in patches from usptream to mitigate the risk of a DoS attack.
* Update the DNS seeds for more reliable bootstrapping of the initial peer discovery.
* Update the list of fixed nodes, in case all other means of peer discovery fail.

## Credits

Listed in strictly alphabetical order, using name listed in Github. This
includes those whose contributions to Bitcoin Core have been merged
into this Dogecoin Core release:

* P. Kaufmann
* Max K.
* Ross Nicoll
* Wladimir J. van der Laan
* Pieter Wuille

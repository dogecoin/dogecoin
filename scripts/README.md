# Scripts Overview
The `scripts` directory consists of the following directories:
- [benchmarking](#benchmarking)
- [buildkite](#buildkite)
- [byron-shelley-allegra-mary](#byron-shelley-allegra-mary)
- [lite](#lite)
- [shelley-from-scratch](#shelley-from-scratch)

#### benchmarking
Contains all the scripts relevant to benchmarking `cardano-node`. See the benchmarking [README](benchmarking/README.md).

#### buildkite
Contains scripts relevant to IOHK's CI.

#### byron-shelley-allegra-mary
Contains a script that sets up a cluster beginning in the Byron era and can transition to the Shelley era. You can also start a cluster in the Shelley, Allegra or Mary era by supplying an argument to `mk-files.sh`.
E.g
```bash
./scripts/byron-to-mary/mk-files.sh shelley # Starts nodes in Shelley era
./scripts/byron-to-mary/mk-files.sh allegra # Starts nodes in Allegra era
./scripts/byron-to-mary/mk-files.sh mary    # Starts nodes in Mary era
```
#### lite
Contains scripts that can start various clusters and intended to be as simple as possible. Note that using the shelley only era testnet clusters breaks compatibility with some cli commands.

#### shelley-from-scratch
Contains a script that creates all the necessary keys etc to create a shelley cluster from scratch.


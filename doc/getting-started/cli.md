# Command Line Interface

In previous tutorials, we have always worked with command `cardano-node`,
but when we built the software from source as described [here](install.md),
we actually installed other executables as well, including the command line interface `cardano-cli`.

This command line interface provides a collection of tools for key generation, transaction construction, certificate creation
and other important tasks.

It is organized in a hierarchy of subcommands, and each level comes with its own built-in documentation of command syntax and options.

We can get the top level help by simply typing the command without arguments:

        cardano-cli

will display available sub-subcommands, one of which is `node`. We can continue drilling down the hierarchy:

        cardano-cli node

and learn about the sub-sub-subcommand `key-gen`. Typing

        cardano-cli node key-gen

will inform us about the parameters this command takes, so we can for example generate a key-pair of offline keys and a file for the issue counter
by typing

        cardano-cli node key-gen \
            --cold-verification-key-file cold.vkey \
            --cold-signing-key-file cold.skey \
            --operational-certificate-issue-counter-file cold.counter

![`cardano-cli` command hierarchy](images/cli.png)

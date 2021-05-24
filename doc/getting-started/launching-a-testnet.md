## Purpose

The `cardano-testnet` tool provides an easy way to launch a testnet for each
of the available modes:

* Byron only
* Shelley only
* Byron-Shelley

## Launching a testnet
To create a testnet, first build the necessary executables:

```bash
$ cabal build cardano-cli cardano-node cardano-node-chairman cardano-testnet
```

Then get a list of available testnets to run:

```bash
$ cd cardano-node-chairman
$ cabal run cardano-testnet
Usage: cardano-testnet COMMAND

Available options:
  -h,--help                Show this help text

Commands:
  byron
  byron-shelley
  shelley
```

Then create the testnet.  For example:

```bash
$ cabal run cardano-testnet byron-shelley
  ✗ <interactive> failed at testnet/Testnet/Run.hs:34:3
    after 1 test.

        ┏━━ src/Testnet/ByronShelley.hs ━━━
     68 ┃ testnet :: H.Conf -> H.Integration [String]
     69 ┃ testnet H.Conf {..} = do
     70 ┃   -- This script sets up a cluster that starts out in Byron, and can transition to Shelley.
     71 ┃   --
...
       ┏━━ testnet/Testnet/Run.hs ━━━
    25 ┃ testnetProperty :: (H.Conf -> H.Integration ()) -> H.Property
    26 ┃ testnetProperty tn = H.integration . H.runFinallies . H.workspace "chairman" $ \tempAbsPath' -> do
       ┃ │ Workspace: /private/var/folders/zh/ln41q4zs52x2fd61rxccmq640000gn/T/chairman/test-acaaa345c8802769
    27 ┃   conf@H.Conf {..} <- H.mkConf tempAbsPath' 42
    28 ┃
    29 ┃   -- Fork a thread to keep alive indefinitely any resources allocated by testnet.
    30 ┃   void . liftResourceT . resourceForkIO . forever . liftIO $ IO.threadDelay 10000000
    31 ┃
    32 ┃   void $ tn conf
    33 ┃
    34 ┃   H.failure -- Intentional failure to force failure report
       ┃   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    This failure can be reproduced by running:
    > recheck (Size 0) (Seed 7516688192894072998 17634780818781912383) <property>

Testnet is running.  Type CTRL-C to exit.
```

At this point, the testnet is running.  The output provides all the information about the testnet including
what socket files are created and what ports are open.  Note that the testnet is launched from the parent
of the workspace directory, so any filenames need to be interpreted in that context.

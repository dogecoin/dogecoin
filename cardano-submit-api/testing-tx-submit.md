# Testing cardano-submit-api

Setting this up for testing and for actual use on a real network.


### Pre-requisites

You will need some a cardano network with payment address and keys.  This may be on `mainnet`, official `testnet`
or a testnet that you've set up yourself.  We will assume these files are in a direction called `playground`:

* `user-1-payment.addr` - User 1 payment address.  This address must have sufficient funds.
* `user-1-payment.vkey` - User 1 verification key.
* `user-2-payment.addr` - User 2 payment address.
* `magic.flag` - The network magic flag.  This will be `--testnet-magic <magic>` or `--mainnet`
  dependening on the network you are using.
* `node.socket` - The socket file for your network, or a symlink to that socket file.  If you
  have a node running (for example, `mainnet` or `testnet` Daedalus), you can the socket file
  using `ps aux | grep cardano-node` and looking for the `--node-socket`.

You will also need to have `yj` installed.  This can be done via `brew`, `apt-get` or `snap`.

### Install and run the cardano-submit-api

Install the `cardani-submit-api`:

```bash
cardano-node $ cabal install cardano-submit-api --overwrite-policy=always
```

Install the `cardani-cli`:

```bash
cardano-node $ cabal install cardano-cli --overwrite-policy=always
```

Copy the configuration yaml to your `playground` and tweak it:

```bash
playground $ cat <configuration-yaml-file> | yj -jy > submit-api-config.yaml
playground $ cat >> submit-api-config.yaml <<EOF
EnableLogMetrics: False
EnableLogging: True
EOF
```

Then run the `cardani-submit-api` against your network:

```bash
playground $ cardano-submit-api --config submit-api-config.yaml --socket-path node.socket --port 8090 $(cat magic.flag)
```

### Build and submit a transaction

In another terminal, find out how much ADA is in your user 1 payment address:

```bash
playground $ CARDANO_NODE_SOCKET_PATH=node.socket cardano-cli query utxo --address $(cat user-1-payment.addr)  --testnet-magic $(cat magic.flag)
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
8a3d63d4d95f669ef62570f2936ad50d2cfad399e04808ca21474e70b11987ee     0        97640000 lovelace
```

Save that date into environment variables for future use.  For example:

```bash
playground $ txhash=8a3d63d4d95f669ef62570f2936ad50d2cfad399e04808ca21474e70b11987ee
playground $ balance=97640000
```

Find out much much will remain after paying `1000000 lovelace` to the target account:

```bash
playground $ remaining=$(echo "$balance - 1000000 - 180000" | bc)
```

Build a raw transaction:

```bash
playground $ cardano-cli transaction build-raw \
  --mary-era \
  --tx-in "$txhash#0" \
  --tx-out "$(cat testnet-user-1-payment.addr)+$remaining" \
  --tx-out "$(cat testnet-user-2-payment.addr)+1000000" \
  --invalid-hereafter "21168607" \
  --fee 180000 \
  --out-file tx.raw
```

Sign the transaction:

```bash
playground $ cardano-cli transaction sign \
  --tx-body-file tx.raw \
  --signing-key-file testnet-user-1-payment.skey \
  $(cat magic.flag) \
  --out-file tx.signed
```

Extract the CBOR binary from the signed transaction:

```bash
playground $ xxd -r -p <<< $(jq .cborHex tx.signed) > tx.signed.cbor
```

Submit the signed transaction using curl:

```bash
playground $ curl --header "Content-Type: application/cbor" -X POST http://localhost:8090/api/submit/tx --data-binary @tx.signed.cbor
"8a3d63d4d95f669ef62570f2936ad50d2cfad399e04808ca21474e70b11987ee"%
```

The string returned is the new transaction hash.

You can check your user 2 payment address has received the funds by querying the following:

```bash
CARDANO_NODE_SOCKET_PATH=node.socket cardano-cli query utxo --address $(cat testnet-user-2-payment.addr)  --testnet-magic $(cat magic.flag)
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
24e12cf8937db7fc95a39ca7780a5a1cb425ee53321d730254d661cc96be572f     1        1000000 lovelace
8a3d63d4d95f669ef62570f2936ad50d2cfad399e04808ca21474e70b11987ee     1        1000000 lovelace
```

Additionally, the `cardano-submit-api` will print out a record of the transaction in its `stdout`:

```
[cardano-tx-submit:Info:26] [2021-03-11 03:32:35.13 UTC] txSubmitPost: successfully submitted transaction 8a3d63d4d95f669e
```

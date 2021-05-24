# Diagnosing transactions problems and troubleshooting

If you experience any problems while setting up your network, we are here to help. Logs created by the nodes will help us classify any problems and suggest solutions. The log levels used are info, debug, and trace.
This section outlines how to diagnose and solve some problems that you may encounter:

**Problem**: Failed reading: Your input is either malformed or not hex encoded:

```$ cardano-cli transaction build-raw \
--shelley-era \
--tx-in f5296e996940b1c81f781594293d423b4250a454f5832b0740a923f74460d34#1 \
--tx-out $(cat payment2.addr)+100000000 \
--tx-out $(cat payment.addr)+899832033 \
--invalid-hereafter 335000 --fee 167965 \
--out-file tx001.raw
```

```> option --tx-in: Failed reading: Your input is either
malformed or not hex encoded:
f5296e996940b1c81f781594293d423b4250a454f5832b0740a923f74460d34
```

**Diagnosis**:  tx-in is not a valid UTXO

**Solution**: Make sure that you are using a correct UTXO. You can query this with:

```$ cardano-cli query utxo \
 --address $(cat payment.addr) \
 --mainnet
```

**Problem**: ExpiredUTxO

```$ cardano-cli transaction submit \
> --tx-file tx001.signed \
> --mainnet
```

```> ApplyTxError [LedgerFailure (UtxowFailure (UtxoFailure(ExpiredUTxO {pfUTXOttl = SlotNo {unSlotNo = 123456}, pfUTXOcurrentSlot = SlotNo {unSlotNo = 123457}})))]
```
**Diagnosis**: The current slot is outside the validity interval

**Solution**: Look at pfUTXOttl and pfUTXOcurrentSlot. Current Slot is ahead of UTXOttl.

Build a new transaction with a invalid-hereafter higher than Current Slot. As a rule of thumb, you will need between 300-500 slots to build, sign, and submit the transaction.

**Problem**: ValueNotConservedUTxO

```$ cardano-cli transaction submit \
> --tx-file tx001.signed \
> --mainnet
```

```ApplyTxError [LedgerFailure (UtxowFailure (UtxoFailure (FeeTooSmallUTxO (Coin 172409) (Coin 167965)))),LedgerFailure (UtxowFailure (UtxoFailure (ValueNotConservedUTxO (Coin 1000000000)(Coin 999999998))))]
```
**Diagnosis**: Value not Conserved: (Input UTXO ≠ Fee + Amount sent + Change)

**Solution**: Check that output amount is equal to input amount

**Problem**: BadInputsUTxO

```$ cardano-cli transaction submit \
--tx-file tx001.signed \
--mainnet
```
```> ApplyTxError [LedgerFailure (UtxowFailure (UtxoFailure (BadInputsUTxO (fromList [TxIn (TxId {_TxId =f5296e996940b1c81f781594293d423b4250a454f5832b0740a923f74460d34e}) ]))))
```
**Diagnosis**: Bad UTXO transaction index

**Solution**: Verify the UTXO transaction index using:

```$ cardano-cli query utxo \
 --address $(cat payment.addr) \
 --mainnet
```

```>  TxHash                                    TxIx      Lovelace
--------------------------------------------------------------------------
7f1d95ce599c84064c61903c0b1334c826b55c48a1.... 1      1000000000000
```

We encourage you to share your ideas and suggestions on our [dedicated support page](https://iohk.zendesk.com/hc/en-us/categories/900000102203-Shelley-Testnet) so that we can create a library of solutions and support for the tasks that you need to perform.

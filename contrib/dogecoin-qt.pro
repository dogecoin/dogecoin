FORMS += \
    ../src/qt/forms/aboutdialog.ui \
    ../src/qt/forms/addressbookpage.ui \
    ../src/qt/forms/askpassphrasedialog.ui \
    ../src/qt/forms/coincontroldialog.ui \
    ../src/qt/forms/editaddressdialog.ui \
    ../src/qt/forms/helpmessagedialog.ui \
    ../src/qt/forms/intro.ui \
    ../src/qt/forms/openuridialog.ui \
    ../src/qt/forms/optionsdialog.ui \
    ../src/qt/forms/overviewpage.ui \
    ../src/qt/forms/receivecoinsdialog.ui \
    ../src/qt/forms/receiverequestdialog.ui \
    ../src/qt/forms/rpcconsole.ui \
    ../src/qt/forms/sendcoinsdialog.ui \
    ../src/qt/forms/sendcoinsentry.ui \
    ../src/qt/forms/signverifymessagedialog.ui \
    ../src/qt/forms/transactiondescdialog.ui \

RESOURCES += \
    ../src/qt/dogecoin.qrc

HEADERS += \
    ../src/json/json_spirit.h \
    ../src/json/json_spirit_error_position.h \
    ../src/json/json_spirit_reader.h \
    ../src/json/json_spirit_reader_template.h \
    ../src/json/json_spirit_stream_reader.h \
    ../src/json/json_spirit_utils.h \
    ../src/json/json_spirit_value.h \
    ../src/json/json_spirit_writer.h \
    ../src/json/json_spirit_writer_template.h \
    ../src/leveldb/db/builder.h \
    ../src/leveldb/db/db_impl.h \
    ../src/leveldb/db/db_iter.h \
    ../src/leveldb/db/dbformat.h \
    ../src/leveldb/db/filename.h \
    ../src/leveldb/db/log_format.h \
    ../src/leveldb/db/log_reader.h \
    ../src/leveldb/db/log_writer.h \
    ../src/leveldb/db/memtable.h \
    ../src/leveldb/db/skiplist.h \
    ../src/leveldb/db/snapshot.h \
    ../src/leveldb/db/table_cache.h \
    ../src/leveldb/db/version_edit.h \
    ../src/leveldb/db/version_set.h \
    ../src/leveldb/db/write_batch_internal.h \
    ../src/leveldb/helpers/memenv/memenv.h \
    ../src/leveldb/include/leveldb/c.h \
    ../src/leveldb/include/leveldb/cache.h \
    ../src/leveldb/include/leveldb/comparator.h \
    ../src/leveldb/include/leveldb/db.h \
    ../src/leveldb/include/leveldb/env.h \
    ../src/leveldb/include/leveldb/filter_policy.h \
    ../src/leveldb/include/leveldb/iterator.h \
    ../src/leveldb/include/leveldb/options.h \
    ../src/leveldb/include/leveldb/slice.h \
    ../src/leveldb/include/leveldb/status.h \
    ../src/leveldb/include/leveldb/table.h \
    ../src/leveldb/include/leveldb/table_builder.h \
    ../src/leveldb/include/leveldb/write_batch.h \
    ../src/leveldb/port/win/stdint.h \
    ../src/leveldb/port/atomic_pointer.h \
    ../src/leveldb/port/port.h \
    ../src/leveldb/port/port_example.h \
    ../src/leveldb/port/port_posix.h \
    ../src/leveldb/port/port_win.h \
    ../src/leveldb/port/thread_annotations.h \
    ../src/leveldb/table/block.h \
    ../src/leveldb/table/block_builder.h \
    ../src/leveldb/table/filter_block.h \
    ../src/leveldb/table/format.h \
    ../src/leveldb/table/iterator_wrapper.h \
    ../src/leveldb/table/merger.h \
    ../src/leveldb/table/two_level_iterator.h \
    ../src/leveldb/util/arena.h \
    ../src/leveldb/util/coding.h \
    ../src/leveldb/util/crc32c.h \
    ../src/leveldb/util/hash.h \
    ../src/leveldb/util/histogram.h \
    ../src/leveldb/util/logging.h \
    ../src/leveldb/util/mutexlock.h \
    ../src/leveldb/util/posix_logger.h \
    ../src/leveldb/util/random.h \
    ../src/leveldb/util/testharness.h \
    ../src/leveldb/util/testutil.h \
    ../src/obj/build.h \
    ../src/qt/forms/ui_aboutdialog.h \
    ../src/qt/forms/ui_addressbookpage.h \
    ../src/qt/forms/ui_askpassphrasedialog.h \
    ../src/qt/forms/ui_coincontroldialog.h \
    ../src/qt/forms/ui_editaddressdialog.h \
    ../src/qt/forms/ui_helpmessagedialog.h \
    ../src/qt/forms/ui_intro.h \
    ../src/qt/forms/ui_openuridialog.h \
    ../src/qt/forms/ui_optionsdialog.h \
    ../src/qt/forms/ui_overviewpage.h \
    ../src/qt/forms/ui_receivecoinsdialog.h \
    ../src/qt/forms/ui_receiverequestdialog.h \
    ../src/qt/forms/ui_rpcconsole.h \
    ../src/qt/forms/ui_sendcoinsdialog.h \
    ../src/qt/forms/ui_sendcoinsentry.h \
    ../src/qt/forms/ui_signverifymessagedialog.h \
    ../src/qt/forms/ui_transactiondescdialog.h \
    ../src/qt/test/bitcoinunitstests.h \
    ../src/qt/test/paymentrequestdata.h \
    ../src/qt/test/paymentservertests.h \
    ../src/qt/test/uritests.h \
    ../src/qt/addressbookpage.h \
    ../src/qt/addresstablemodel.h \
    ../src/qt/askpassphrasedialog.h \
    ../src/qt/bitcoinaddressvalidator.h \
    ../src/qt/bitcoinamountfield.h \
    ../src/qt/bitcoingui.h \
    ../src/qt/bitcoinunits.h \
    ../src/qt/clientmodel.h \
    ../src/qt/coincontroldialog.h \
    ../src/qt/coincontroltreewidget.h \
    ../src/qt/csvmodelwriter.h \
    ../src/qt/editaddressdialog.h \
    ../src/qt/guiconstants.h \
    ../src/qt/guiutil.h \
    ../src/qt/intro.h \
    ../src/qt/macdockiconhandler.h \
    ../src/qt/macnotificationhandler.h \
    ../src/qt/monitoreddatamapper.h \
    ../src/qt/notificator.h \
    ../src/qt/openuridialog.h \
    ../src/qt/optionsdialog.h \
    ../src/qt/optionsmodel.h \
    ../src/qt/overviewpage.h \
    ../src/qt/paymentrequest.pb.h \
    ../src/qt/paymentrequestplus.h \
    ../src/qt/paymentserver.h \
    ../src/qt/qvalidatedlineedit.h \
    ../src/qt/qvaluecombobox.h \
    ../src/qt/receivecoinsdialog.h \
    ../src/qt/receiverequestdialog.h \
    ../src/qt/recentrequeststablemodel.h \
    ../src/qt/rpcconsole.h \
    ../src/qt/sendcoinsdialog.h \
    ../src/qt/sendcoinsentry.h \
    ../src/qt/signverifymessagedialog.h \
    ../src/qt/splashscreen.h \
    ../src/qt/trafficgraphwidget.h \
    ../src/qt/transactiondesc.h \
    ../src/qt/transactiondescdialog.h \
    ../src/qt/transactionfilterproxy.h \
    ../src/qt/transactionrecord.h \
    ../src/qt/transactiontablemodel.h \
    ../src/qt/transactionview.h \
    ../src/qt/utilitydialog.h \
    ../src/qt/walletframe.h \
    ../src/qt/walletmodel.h \
    ../src/qt/walletmodeltransaction.h \
    ../src/qt/walletview.h \
    ../src/addrman.h \
    ../src/alert.h \
    ../src/allocators.h \
    ../src/base58.h \
    ../src/bignum.h \
    ../src/bitcoin-config.h \
    ../src/bloom.h \
    ../src/chainparams.h \
    ../src/checkpoints.h \
    ../src/checkqueue.h \
    ../src/clientversion.h \
    ../src/coincontrol.h \
    ../src/coins.h \
    ../src/compat.h \
    ../src/core.h \
    ../src/crypter.h \
    ../src/db.h \
    ../src/hash.h \
    ../src/init.h \
    ../src/key.h \
    ../src/keystore.h \
    ../src/leveldbwrapper.h \
    ../src/limitedmap.h \
    ../src/main.h \
    ../src/miner.h \
    ../src/mruset.h \
    ../src/net.h \
    ../src/netbase.h \
    ../src/noui.h \
    ../src/protocol.h \
    ../src/rpcclient.h \
    ../src/rpcprotocol.h \
    ../src/rpcserver.h \
    ../src/scriptutils.h \
    ../src/scrypt.h \
    ../src/serialize.h \
    ../src/sync.h \
    ../src/threadsafety.h \
    ../src/tinyformat.h \
    ../src/txdb.h \
    ../src/txmempool.h \
    ../src/ui_interface.h \
    ../src/uint256.h \
    ../src/util.h \
    ../src/version.h \
    ../src/wallet.h \
    ../src/walletdb.h

SOURCES += \
    ../src/json/json_spirit_reader.cpp \
    ../src/json/json_spirit_value.cpp \
    ../src/json/json_spirit_writer.cpp \
    ../src/leveldb/db/autocompact_test.cc \
    ../src/leveldb/db/builder.cc \
    ../src/leveldb/db/c.cc \
    ../src/leveldb/db/corruption_test.cc \
    ../src/leveldb/db/db_bench.cc \
    ../src/leveldb/db/db_impl.cc \
    ../src/leveldb/db/db_iter.cc \
    ../src/leveldb/db/db_test.cc \
    ../src/leveldb/db/dbformat.cc \
    ../src/leveldb/db/dbformat_test.cc \
    ../src/leveldb/db/filename.cc \
    ../src/leveldb/db/filename_test.cc \
    ../src/leveldb/db/leveldb_main.cc \
    ../src/leveldb/db/log_reader.cc \
    ../src/leveldb/db/log_test.cc \
    ../src/leveldb/db/log_writer.cc \
    ../src/leveldb/db/memtable.cc \
    ../src/leveldb/db/repair.cc \
    ../src/leveldb/db/skiplist_test.cc \
    ../src/leveldb/db/table_cache.cc \
    ../src/leveldb/db/version_edit.cc \
    ../src/leveldb/db/version_edit_test.cc \
    ../src/leveldb/db/version_set.cc \
    ../src/leveldb/db/version_set_test.cc \
    ../src/leveldb/db/write_batch.cc \
    ../src/leveldb/db/write_batch_test.cc \
    ../src/leveldb/doc/bench/db_bench_sqlite3.cc \
    ../src/leveldb/doc/bench/db_bench_tree_db.cc \
    ../src/leveldb/helpers/memenv/memenv.cc \
    ../src/leveldb/helpers/memenv/memenv_test.cc \
    ../src/leveldb/issues/issue178_test.cc \
    ../src/leveldb/issues/issue200_test.cc \
    ../src/leveldb/port/port_posix.cc \
    ../src/leveldb/port/port_win.cc \
    ../src/leveldb/table/block.cc \
    ../src/leveldb/table/block_builder.cc \
    ../src/leveldb/table/filter_block.cc \
    ../src/leveldb/table/filter_block_test.cc \
    ../src/leveldb/table/format.cc \
    ../src/leveldb/table/iterator.cc \
    ../src/leveldb/table/merger.cc \
    ../src/leveldb/table/table.cc \
    ../src/leveldb/table/table_builder.cc \
    ../src/leveldb/table/table_test.cc \
    ../src/leveldb/table/two_level_iterator.cc \
    ../src/leveldb/util/arena.cc \
    ../src/leveldb/util/arena_test.cc \
    ../src/leveldb/util/bloom.cc \
    ../src/leveldb/util/bloom_test.cc \
    ../src/leveldb/util/cache.cc \
    ../src/leveldb/util/cache_test.cc \
    ../src/leveldb/util/coding.cc \
    ../src/leveldb/util/coding_test.cc \
    ../src/leveldb/util/comparator.cc \
    ../src/leveldb/util/crc32c.cc \
    ../src/leveldb/util/crc32c_test.cc \
    ../src/leveldb/util/env.cc \
    ../src/leveldb/util/env_posix.cc \
    ../src/leveldb/util/env_test.cc \
    ../src/leveldb/util/env_win.cc \
    ../src/leveldb/util/filter_policy.cc \
    ../src/leveldb/util/hash.cc \
    ../src/leveldb/util/histogram.cc \
    ../src/leveldb/util/logging.cc \
    ../src/leveldb/util/options.cc \
    ../src/leveldb/util/status.cc \
    ../src/leveldb/util/testharness.cc \
    ../src/leveldb/util/testutil.cc \
    ../src/qt/test/bitcoinunitstests.cpp \
    ../src/qt/test/paymentservertests.cpp \
    ../src/qt/test/test_main.cpp \
    ../src/qt/test/uritests.cpp \
    ../src/qt/addressbookpage.cpp \
    ../src/qt/addresstablemodel.cpp \
    ../src/qt/askpassphrasedialog.cpp \
    ../src/qt/bitcoin.cpp \
    ../src/qt/bitcoinaddressvalidator.cpp \
    ../src/qt/bitcoinamountfield.cpp \
    ../src/qt/bitcoingui.cpp \
    ../src/qt/bitcoinstrings.cpp \
    ../src/qt/bitcoinunits.cpp \
    ../src/qt/clientmodel.cpp \
    ../src/qt/coincontroldialog.cpp \
    ../src/qt/coincontroltreewidget.cpp \
    ../src/qt/csvmodelwriter.cpp \
    ../src/qt/editaddressdialog.cpp \
    ../src/qt/guiutil.cpp \
    ../src/qt/intro.cpp \
    ../src/qt/moc_addressbookpage.cpp \
    ../src/qt/moc_addresstablemodel.cpp \
    ../src/qt/moc_askpassphrasedialog.cpp \
    ../src/qt/moc_bitcoinaddressvalidator.cpp \
    ../src/qt/moc_bitcoinamountfield.cpp \
    ../src/qt/moc_bitcoingui.cpp \
    ../src/qt/moc_bitcoinunits.cpp \
    ../src/qt/moc_clientmodel.cpp \
    ../src/qt/moc_coincontroldialog.cpp \
    ../src/qt/moc_coincontroltreewidget.cpp \
    ../src/qt/moc_csvmodelwriter.cpp \
    ../src/qt/moc_editaddressdialog.cpp \
    ../src/qt/moc_guiutil.cpp \
    ../src/qt/moc_intro.cpp \
    ../src/qt/moc_macdockiconhandler.cpp \
    ../src/qt/moc_macnotificationhandler.cpp \
    ../src/qt/moc_monitoreddatamapper.cpp \
    ../src/qt/moc_notificator.cpp \
    ../src/qt/moc_openuridialog.cpp \
    ../src/qt/moc_optionsdialog.cpp \
    ../src/qt/moc_optionsmodel.cpp \
    ../src/qt/moc_overviewpage.cpp \
    ../src/qt/moc_paymentserver.cpp \
    ../src/qt/moc_qvalidatedlineedit.cpp \
    ../src/qt/moc_qvaluecombobox.cpp \
    ../src/qt/moc_receivecoinsdialog.cpp \
    ../src/qt/moc_receiverequestdialog.cpp \
    ../src/qt/moc_recentrequeststablemodel.cpp \
    ../src/qt/moc_rpcconsole.cpp \
    ../src/qt/moc_sendcoinsdialog.cpp \
    ../src/qt/moc_sendcoinsentry.cpp \
    ../src/qt/moc_signverifymessagedialog.cpp \
    ../src/qt/moc_splashscreen.cpp \
    ../src/qt/moc_trafficgraphwidget.cpp \
    ../src/qt/moc_transactiondesc.cpp \
    ../src/qt/moc_transactiondescdialog.cpp \
    ../src/qt/moc_transactionfilterproxy.cpp \
    ../src/qt/moc_transactiontablemodel.cpp \
    ../src/qt/moc_transactionview.cpp \
    ../src/qt/moc_utilitydialog.cpp \
    ../src/qt/moc_walletframe.cpp \
    ../src/qt/moc_walletmodel.cpp \
    ../src/qt/moc_walletview.cpp \
    ../src/qt/monitoreddatamapper.cpp \
    ../src/qt/notificator.cpp \
    ../src/qt/openuridialog.cpp \
    ../src/qt/optionsdialog.cpp \
    ../src/qt/optionsmodel.cpp \
    ../src/qt/overviewpage.cpp \
    ../src/qt/paymentrequest.pb.cc \
    ../src/qt/paymentrequestplus.cpp \
    ../src/qt/paymentserver.cpp \
    ../src/qt/qrc_dogecoin.cpp \
    ../src/qt/qvalidatedlineedit.cpp \
    ../src/qt/qvaluecombobox.cpp \
    ../src/qt/receivecoinsdialog.cpp \
    ../src/qt/receiverequestdialog.cpp \
    ../src/qt/recentrequeststablemodel.cpp \
    ../src/qt/rpcconsole.cpp \
    ../src/qt/sendcoinsdialog.cpp \
    ../src/qt/sendcoinsentry.cpp \
    ../src/qt/signverifymessagedialog.cpp \
    ../src/qt/splashscreen.cpp \
    ../src/qt/trafficgraphwidget.cpp \
    ../src/qt/transactiondesc.cpp \
    ../src/qt/transactiondescdialog.cpp \
    ../src/qt/transactionfilterproxy.cpp \
    ../src/qt/transactionrecord.cpp \
    ../src/qt/transactiontablemodel.cpp \
    ../src/qt/transactionview.cpp \
    ../src/qt/utilitydialog.cpp \
    ../src/qt/walletframe.cpp \
    ../src/qt/walletmodel.cpp \
    ../src/qt/walletmodeltransaction.cpp \
    ../src/qt/walletview.cpp \
    ../src/test/accounting_tests.cpp \
    ../src/test/alert_tests.cpp \
    ../src/test/allocator_tests.cpp \
    ../src/test/base32_tests.cpp \
    ../src/test/base58_tests.cpp \
    ../src/test/base64_tests.cpp \
    ../src/test/bignum_tests.cpp \
    ../src/test/bip32_tests.cpp \
    ../src/test/bloom_tests.cpp \
    ../src/test/canonical_tests.cpp \
    ../src/test/checkblock_tests.cpp \
    ../src/test/Checkpoints_tests.cpp \
    ../src/test/compress_tests.cpp \
    ../src/test/DoS_tests.cpp \
    ../src/test/getarg_tests.cpp \
    ../src/test/hash_tests.cpp \
    ../src/test/hmac_tests.cpp \
    ../src/test/key_tests.cpp \
    ../src/test/main_tests.cpp \
    ../src/test/miner_tests.cpp \
    ../src/test/mruset_tests.cpp \
    ../src/test/multisig_tests.cpp \
    ../src/test/netbase_tests.cpp \
    ../src/test/pmt_tests.cpp \
    ../src/test/rpc_tests.cpp \
    ../src/test/rpc_wallet_tests.cpp \
    ../src/test/script_P2SH_tests.cpp \
    ../src/test/script_tests.cpp \
    ../src/test/scrypt_tests.cpp \
    ../src/test/serialize_tests.cpp \
    ../src/test/sighash_tests.cpp \
    ../src/test/sigopcount_tests.cpp \
    ../src/test/test_dogecoin.cpp \
    ../src/test/transaction_tests.cpp \
    ../src/test/uint256_tests.cpp \
    ../src/test/util_tests.cpp \
    ../src/test/wallet_tests.cpp \
    ../src/addrman.cpp \
    ../src/alert.cpp \
    ../src/allocators.cpp \
    ../src/bloom.cpp \
    ../src/chainparams.cpp \
    ../src/checkpoints.cpp \
    ../src/coins.cpp \
    ../src/core.cpp \
    ../src/crypter.cpp \
    ../src/db.cpp \
    ../src/dogecoin-cli.cpp \
    ../src/dogecoind.cpp \
    ../src/hash.cpp \
    ../src/init.cpp \
    ../src/key.cpp \
    ../src/keystore.cpp \
    ../src/leveldbwrapper.cpp \
    ../src/main.cpp \
    ../src/miner.cpp \
    ../src/net.cpp \
    ../src/netbase.cpp \
    ../src/noui.cpp \
    ../src/protocol.cpp \
    ../src/rpcblockchain.cpp \
    ../src/rpcclient.cpp \
    ../src/rpcdump.cpp \
    ../src/rpcmining.cpp \
    ../src/rpcmisc.cpp \
    ../src/rpcnet.cpp \
    ../src/rpcprotocol.cpp \
    ../src/rpcrawtransaction.cpp \
    ../src/rpcserver.cpp \
    ../src/rpcwallet.cpp \
    ../src/script.cpp \
    ../src/scrypt-sse2.cpp \
    ../src/scrypt.cpp \
    ../src/sync.cpp \
    ../src/txdb.cpp \
    ../src/txmempool.cpp \
    ../src/util.cpp \
    ../src/version.cpp \
    ../src/wallet.cpp \
    ../src/walletdb.cpp \
    ../src/leveldb/db/c_test.c

QT += \
    printsupport

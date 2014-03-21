include $(top_srcdir)/src/Makefile.include

AM_CPPFLAGS += -I$(top_srcdir)/src \
  -I$(top_srcdir)/src/qt \
  -I$(top_builddir)/src/qt \
  $(PROTOBUF_CFLAGS) \
    $(QR_CFLAGS)
bin_PROGRAMS = test_dogecoin-qt
TESTS = test_dogecoin-qt

TEST_QT_MOC_CPP = \
  moc_bitcoinunitstests.cpp \
  moc_uritests.cpp

if ENABLE_WALLET
TEST_QT_MOC_CPP += moc_paymentservertests.cpp
endif

TEST_QT_H = \
  bitcoinunitstests.h \
  uritests.h \
  paymentrequestdata.h \
  paymentservertests.h

BUILT_SOURCES = $(TEST_QT_MOC_CPP)

test_dogecoin_qt_CPPFLAGS = $(AM_CPPFLAGS) $(QT_INCLUDES) $(QT_TEST_INCLUDES)

test_dogecoin_qt_SOURCES = \
  bitcoinunitstests.cpp \
  test_main.cpp \
  uritests.cpp \
  $(TEST_QT_H)
if ENABLE_WALLET
test_dogecoin_qt_SOURCES += \
  paymentservertests.cpp
endif

nodist_test_dogecoin_qt_SOURCES = $(TEST_QT_MOC_CPP)

test_dogecoin_qt_LDADD = $(LIBBITCOINQT)
if ENABLE_WALLET
test_dogecoin_qt_LDADD += $(LIBBITCOIN_WALLET) $(LIBBITCOIN_SERVER_WALLET)
endif
test_dogecoin_qt_LDADD += $(LIBBITCOIN_SERVER)
if ENABLE_WALLET
test_dogecoin_qt_LDADD += $(LIBBITCOIN_SERVER_WALLET)
endif
test_dogecoin_qt_LDADD += $(LIBBITCOIN_CLI) $(LIBBITCOIN_COMMON) \
  ${RPCWALLET} $(LIBLEVELDB) \
  $(LIBMEMENV) $(BOOST_LIBS) $(QT_DBUS_LIBS) $(QT_TEST_LIBS) $(QT_LIBS) \
  $(QR_LIBS) $(PROTOBUF_LIBS) $(BDB_LIBS)
test_dogecoin_qt_LDFLAGS = $(QT_LDFLAGS)

CLEANFILES = $(BUILT_SOURCES) *.gcda *.gcno

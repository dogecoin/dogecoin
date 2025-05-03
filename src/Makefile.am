AM_CPPFLAGS = \
  -I$(srcdir)/obj \
  -I$(builddir)/obj \
  -I$(srcdir)/univalue/include \
  -I$(srcdir)/leveldb/include \
  -I$(srcdir)/leveldb/helpers/memenv \
  $(BOOST_CPPFLAGS) \
  $(CRYPTO_CFLAGS) \
  $(SSL_CFLAGS)

LIBBITCOIN_SERVER = \
  amount.cpp \
  $(BITCOIN_CORE_OBJS)

BITCOIN_TESTS = \
  test/amount_tests.cpp \
  $(BITCOIN_TEST_OBJS)

noinst_PROGRAMS = test/test_dogecoin

test_test_dogecoin_SOURCES = \
  $(BITCOIN_TESTS)

test_test_dogecoin_CPPFLAGS = $(AM_CPPFLAGS)
test_test_dogecoin_LDADD = $(LIBBITCOIN_SERVER) $(LIBS) $(BOOST_LDFLAGS) $(BOOST_UNIT_TEST_FRAMEWORK_LIB)

EXTRA_DIST = $(srcdir)/amount.h
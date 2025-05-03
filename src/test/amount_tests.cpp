#include "amount.h"

#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_SUITE(AmountTests)

BOOST_AUTO_TEST_CASE(FormatMoneyTests) {
    BOOST_CHECK_EQUAL(FormatMoney(123456789), "1.23456789");
    BOOST_CHECK_EQUAL(FormatMoney(0), "0.00000000");
    BOOST_CHECK_EQUAL(FormatMoney(COIN), "1.00000000");
}

BOOST_AUTO_TEST_CASE(ParseMoneyTests) {
    CAmount n;
    BOOST_CHECK(ParseMoney("1.23456789", n));
    BOOST_CHECK_EQUAL(n, 123456789);
    
    BOOST_CHECK(ParseMoney("0", n));
    BOOST_CHECK_EQUAL(n, 0);

    BOOST_CHECK(!ParseMoney("invalid", n));
}

BOOST_AUTO_TEST_SUITE_END()

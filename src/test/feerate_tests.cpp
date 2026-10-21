// Copyright (c) 2024 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "amount.h"
#include "test/test_bitcoin.h"

#include <boost/test/unit_test.hpp>

BOOST_FIXTURE_TEST_SUITE(feerate_tests, BasicTestingSetup)

BOOST_AUTO_TEST_CASE(GetRelayFeeTest)
{
    // Test that GetRelayFee rounds up for positive fee rates
    CFeeRate feeRate(1000); // 1000 sat/kB

    // Exact: 1000 bytes = 1000 sat, no rounding needed
    BOOST_CHECK_EQUAL(feeRate.GetRelayFee(1000), 1000);

    // 1 byte: 1000/1000 = 1 sat, exact
    BOOST_CHECK_EQUAL(feeRate.GetRelayFee(1), 1);

    // 500 bytes: 500 sat, exact
    BOOST_CHECK_EQUAL(feeRate.GetRelayFee(500), 500);

    // Zero bytes should return zero
    BOOST_CHECK_EQUAL(feeRate.GetRelayFee(0), 0);

    // Test rounding up with non-even fee rate
    CFeeRate feeRate2(123); // 123 sat/kB

    // 1 byte: 123*1/1000 = 0.123 -> wallet truncates to 0, but relay rounds up to 1
    BOOST_CHECK_EQUAL(feeRate2.GetRelayFee(1), 1);

    // 8 bytes: 123*8/1000 = 0.984 -> GetFee returns 1 (special case), GetRelayFee should also return 1
    BOOST_CHECK_EQUAL(feeRate2.GetRelayFee(8), 1);

    // 9 bytes: 123*9/1000 = 1.107 -> truncates to 1, relay rounds up to 2
    BOOST_CHECK_EQUAL(feeRate2.GetRelayFee(9), 2);

    // 121 bytes: 123*121/1000 = 14.883 -> truncates to 14, relay rounds up to 15
    BOOST_CHECK_EQUAL(feeRate2.GetRelayFee(121), 15);

    // 1000 bytes: exact, 123
    BOOST_CHECK_EQUAL(feeRate2.GetRelayFee(1000), 123);

    // Compare with GetFee to verify relay is always >= wallet fee
    for (size_t nBytes = 0; nBytes < 2000; nBytes += 7) {
        BOOST_CHECK(feeRate2.GetRelayFee(nBytes) >= feeRate2.GetFee(nBytes));
    }

    // Test with zero fee rate
    CFeeRate zeroRate(0);
    BOOST_CHECK_EQUAL(zeroRate.GetRelayFee(0), 0);
    BOOST_CHECK_EQUAL(zeroRate.GetRelayFee(100), 0);
    BOOST_CHECK_EQUAL(zeroRate.GetRelayFee(1000), 0);

    // Test with negative fee rate (should not round up)
    CFeeRate negRate(-1000);
    BOOST_CHECK_EQUAL(negRate.GetRelayFee(0), 0);
    BOOST_CHECK_EQUAL(negRate.GetRelayFee(1), -1);
    BOOST_CHECK_EQUAL(negRate.GetRelayFee(500), -500);
    BOOST_CHECK_EQUAL(negRate.GetRelayFee(1000), -1000);

    // Large transaction: 100kB at 1000 sat/kB = 100,000 sat
    BOOST_CHECK_EQUAL(feeRate.GetRelayFee(100000), 100000);
}

BOOST_AUTO_TEST_CASE(FeeRateComparisonOperatorsTest)
{
    CFeeRate a(100);
    CFeeRate b(200);
    CFeeRate c(100);

    // Test operator==
    BOOST_CHECK(a == c);
    BOOST_CHECK(!(a == b));

    // Test operator!=
    BOOST_CHECK(a != b);
    BOOST_CHECK(!(a != c));

    // Test operator<
    BOOST_CHECK(a < b);
    BOOST_CHECK(!(b < a));
    BOOST_CHECK(!(a < c));

    // Test operator>
    BOOST_CHECK(b > a);
    BOOST_CHECK(!(a > b));
    BOOST_CHECK(!(a > c));

    // Test operator<=
    BOOST_CHECK(a <= b);
    BOOST_CHECK(a <= c);
    BOOST_CHECK(!(b <= a));

    // Test operator>=
    BOOST_CHECK(b >= a);
    BOOST_CHECK(a >= c);
    BOOST_CHECK(!(a >= b));

    // Test with negative values
    CFeeRate neg(-100);
    BOOST_CHECK(neg < a);
    BOOST_CHECK(neg != a);
    BOOST_CHECK(a > neg);

    // Test with zero
    CFeeRate zero(0);
    BOOST_CHECK(neg < zero);
    BOOST_CHECK(zero < a);
    BOOST_CHECK(zero != a);
    BOOST_CHECK(zero != neg);
}

BOOST_AUTO_TEST_CASE(FeeRateArithmeticOperatorsTest)
{
    CFeeRate a(1000);
    CFeeRate b(500);

    // Test operator+
    CFeeRate sum = a + b;
    BOOST_CHECK_EQUAL(sum.GetFeePerK(), 1500);

    // Test operator-
    CFeeRate diff = a - b;
    BOOST_CHECK_EQUAL(diff.GetFeePerK(), 500);

    // Test operator+=
    CFeeRate c(100);
    c += CFeeRate(50);
    BOOST_CHECK_EQUAL(c.GetFeePerK(), 150);

    // Test operator-=
    CFeeRate d(300);
    d -= CFeeRate(100);
    BOOST_CHECK_EQUAL(d.GetFeePerK(), 200);

    // Test chaining +=
    CFeeRate e(0);
    e += CFeeRate(100);
    e += CFeeRate(200);
    e += CFeeRate(300);
    BOOST_CHECK_EQUAL(e.GetFeePerK(), 600);

    // Test subtraction to negative
    CFeeRate f(100);
    CFeeRate g = f - CFeeRate(200);
    BOOST_CHECK_EQUAL(g.GetFeePerK(), -100);

    // Test with negative operands
    CFeeRate neg(-500);
    CFeeRate result = a + neg;
    BOOST_CHECK_EQUAL(result.GetFeePerK(), 500);

    // Test symmetry: a + b == b + a
    BOOST_CHECK(a + b == b + a);

    // Test identity: a + 0 == a
    BOOST_CHECK(a + CFeeRate(0) == a);

    // Test inverse: a - a == 0
    BOOST_CHECK((a - a) == CFeeRate(0));
}

BOOST_AUTO_TEST_CASE(FeeRateToStringTest)
{
    // Test ToString output format
    CFeeRate feeRate(COIN); // 1 DOGE per kB
    std::string str = feeRate.ToString();
    BOOST_CHECK(str.find("1.00000000") != std::string::npos);
    BOOST_CHECK(str.find("DOGE/kB") != std::string::npos);

    // Test zero rate
    CFeeRate zeroRate(0);
    str = zeroRate.ToString();
    BOOST_CHECK(str.find("0.00000000") != std::string::npos);

    // Test small rate
    CFeeRate smallRate(1);
    str = smallRate.ToString();
    BOOST_CHECK(str.find("0.00000001") != std::string::npos);
}

BOOST_AUTO_TEST_CASE(FeeRateConstructorEdgeCasesTest)
{
    // Test construction from fee paid and size
    // 1000 sat paid for 1000 bytes = 1000 sat/kB
    CFeeRate rate1(CAmount(1000), 1000);
    BOOST_CHECK_EQUAL(rate1.GetFeePerK(), 1000);

    // 1 sat paid for 1 byte = 1000 sat/kB
    CFeeRate rate2(CAmount(1), 1);
    BOOST_CHECK_EQUAL(rate2.GetFeePerK(), 1000);

    // 0 bytes should not crash, result in 0 rate
    CFeeRate rate3(CAmount(1000), 0);
    BOOST_CHECK_EQUAL(rate3.GetFeePerK(), 0);

    // Very large size
    CFeeRate rate4(CAmount(1000), 1000000);
    BOOST_CHECK_EQUAL(rate4.GetFeePerK(), 1);

    // Negative fee
    CFeeRate rate5(CAmount(-1000), 1000);
    BOOST_CHECK_EQUAL(rate5.GetFeePerK(), -1000);

    // Very small fee for large size (precision loss)
    CFeeRate rate6(CAmount(1), 2000);
    BOOST_CHECK_EQUAL(rate6.GetFeePerK(), 0);

    CFeeRate rate7(CAmount(2), 2000);
    BOOST_CHECK_EQUAL(rate7.GetFeePerK(), 1000 * 2 / 2000); // = 1
}

BOOST_AUTO_TEST_CASE(FeeRateGetFeeEdgeCasesTest)
{
    // Test boundary between special case (returns 1) and normal calculation
    CFeeRate rate(1); // 1 sat/kB

    // 0 bytes -> 0
    BOOST_CHECK_EQUAL(rate.GetFee(0), 0);

    // 1 byte -> 1*1/1000 = 0, but special case returns 1
    BOOST_CHECK_EQUAL(rate.GetFee(1), 1);

    // 999 bytes -> 1*999/1000 = 0, special case returns 1
    BOOST_CHECK_EQUAL(rate.GetFee(999), 1);

    // 1000 bytes -> exactly 1
    BOOST_CHECK_EQUAL(rate.GetFee(1000), 1);

    // 1001 bytes -> 1*1001/1000 = 1
    BOOST_CHECK_EQUAL(rate.GetFee(1001), 1);

    // 2000 bytes -> 2
    BOOST_CHECK_EQUAL(rate.GetFee(2000), 2);

    // Similarly for negative rate
    CFeeRate negRate(-1);
    BOOST_CHECK_EQUAL(negRate.GetFee(0), 0);
    BOOST_CHECK_EQUAL(negRate.GetFee(1), -1);
    BOOST_CHECK_EQUAL(negRate.GetFee(999), -1);
    BOOST_CHECK_EQUAL(negRate.GetFee(1000), -1);
    BOOST_CHECK_EQUAL(negRate.GetFee(2000), -2);

    // Large fee rate with large size
    CFeeRate largeRate(MAX_MONEY / 1000); // avoid overflow in GetFee for small sizes
    BOOST_CHECK(largeRate.GetFee(1) > 0);
    BOOST_CHECK(largeRate.GetFee(1000) > 0);
}

BOOST_AUTO_TEST_CASE(MoneyRangeTest)
{
    // Test MoneyRange boundary conditions
    BOOST_CHECK(MoneyRange(0));
    BOOST_CHECK(MoneyRange(1));
    BOOST_CHECK(MoneyRange(MAX_MONEY));
    BOOST_CHECK(MoneyRange(MAX_MONEY - 1));

    // Out of range
    BOOST_CHECK(!MoneyRange(-1));
    BOOST_CHECK(!MoneyRange(MAX_MONEY + 1));
    BOOST_CHECK(!MoneyRange(-MAX_MONEY));

    // Dogecoin-specific: MAX_MONEY is 10 billion DOGE
    BOOST_CHECK_EQUAL(MAX_MONEY, int64_t(10000000000) * COIN);
}

BOOST_AUTO_TEST_SUITE_END()

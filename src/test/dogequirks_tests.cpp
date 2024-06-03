#include "dogequirks.h"
#include <test/util/setup_common.h>
#include <boost/test/unit_test.hpp>

BOOST_FIXTURE_TEST_SUITE(dogequirks_tests, BasicTestingSetup)

inline int date_test(const char *date)
{
int year;
int month;
int day;
sscanf(date, "%2d/%2d/%4d", &month,&day,&year);
int phase=CDogeQuirks::moon_phase(year,month,day);
return phase;
}


BOOST_AUTO_TEST_CASE(data_access_test)
{
//Test some "random" moon dates to see everything is working

//Date of creating this test case

BOOST_CHECK(date_test("5/23/2021") == 3); // not quite the full moon yet
BOOST_CHECK(date_test("5/25/2008") == 6); // #GeekPower
BOOST_CHECK(date_test("5/26/2021") == 4); // Actual full moon (and full lunar eclipse too!)
BOOST_CHECK(date_test("5/28/2021") == 5); // Just after the full moon
BOOST_CHECK(date_test("5/16/1976") == 5); // somebodys birthday maybe?
BOOST_CHECK(date_test("2/29/1980") == 4); // birth of an awesome leap year baby
BOOST_CHECK(date_test("6/22/2002") == 3); // wedding day
BOOST_CHECK(date_test("5/4/2011") == 1);  // RD2D/C3PO


}

BOOST_AUTO_TEST_SUITE_END()

